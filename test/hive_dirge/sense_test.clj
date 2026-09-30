(ns hive-dirge.sense-test
  "The sense relay: sixth-sense senses become loop ops on the dirge SSE feed.
   Pure policy/prompt/pending first, then the host end to end on loopback
   with a RECORDING ISenseSource in place of sixth-sense."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-addon.protocol :as addon]
            [hive-dirge.host :as host]
            [hive-dirge.host.domain :as host-domain]
            [hive-dirge.sense.domain :as domain]
            [hive-dirge.sense.ports :as ports]
            [hive-dirge.sense.relay :as relay]
            [hive-vessel.wire :as wire])
  (:import (java.net URI)
           (java.net.http HttpClient HttpClient$Version HttpRequest
                          HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration)
           (java.util.concurrent LinkedBlockingQueue TimeUnit)))

(defn- sense [id cls & {:as more}]
  (merge {:sense/id id :sense/class cls :sense/agent "ling-1" :sense/project "p"
          :sense/parent "coord" :sense/text (str "text of " id) :sense/at 7}
         more))

;; =============================================================================
;; Pure
;; =============================================================================

(deftest policy-defaults-and-loose-overrides
  (is (= :steer (domain/mode-of domain/default-policy (sense "a" :sense/blocked))))
  (is (= :interject (domain/mode-of domain/default-policy (sense "a" :sense/context-death))))
  (is (= :followup (domain/mode-of domain/default-policy (sense "a" :sense/completed))))
  (is (= :followup (domain/mode-of domain/default-policy (sense "a" :sense/custom)))
      "a class the policy does not name is news, never dropped")
  (let [p (domain/->policy {"completed" "steer" :error :ignore :sense/ask "bogus" ":blocked" ":interject"})]
    (is (= :steer (p :sense/completed)))
    (is (= :ignore (p :sense/error)))
    (is (= :interject (p :sense/blocked)))
    (is (= :steer (p :sense/ask)) "an unknown mode is dropped, the default stays")))

(deftest sense-becomes-a-loop-op
  (let [op (domain/sense->op domain/default-policy (sense "ask-9" :sense/ask :sense/text "which db?"))]
    (is (= {"op" "loop/steer" "id" "ask-9" "class" "ask" "agent" "ling-1" "project" "p"
            "parent" "coord" "at" 7 "text" "which db?" "reply_to" "ask-9"}
           (dissoc op "prompt")))
    (is (str/starts-with? (get op "prompt") "[hive sense · ling-1 asks]\nwhich db?"))
    (is (str/includes? (get op "prompt") "\"ss reply\", to \"ask-9\"")
        "an ask is answered by its ask id"))
  (testing "a blocked ling is unblocked by agent id"
    (let [op (domain/sense->op domain/default-policy (sense "b" :sense/blocked))]
      (is (= "ling-1" (get op "reply_to")))
      (is (str/includes? (get op "prompt") "to \"ling-1\""))))
  (testing "a completion carries no instruction"
    (is (= "[hive sense · ling-1 completed]\ntext of c"
           (get (domain/sense->op domain/default-policy (sense "c" :sense/completed)) "prompt"))))
  (is (nil? (domain/sense->op (domain/->policy {"error" "ignore"}) (sense "e" :sense/error)))))

(deftest pending-holds-until-acked-and-is-bounded
  (let [op #(hash-map "id" %)
        p (reduce domain/hold domain/empty-pending (map op ["a" "b" "c"]))]
    (is (= ["a" "b" "c"] (map #(get % "id") (domain/unacked p))))
    (is (= ["a" "c"] (map #(get % "id") (domain/unacked (domain/ack p "b")))))
    (is (= p (domain/ack p "zz")))
    (is (= ["a" "b" "c"] (map #(get % "id") (domain/unacked (domain/hold p (op "a")))))
        "re-holding keeps the first-sent position")
    (is (= ["b" "c" "d"] (map #(get % "id") (domain/unacked (domain/hold p (op "d") 3))))
        "past the cap the oldest is forgotten")))

(deftest ack-reply-parses
  (is (= {:command :sense/ack :sense-id "s1"}
         (host-domain/parse-reply "{\"action\":\"ack\",\"target\":\"s1\"}")))
  (is (= :reply/missing-target (:reply/error (host-domain/parse-reply "{\"action\":\"ack\"}")))))

;; =============================================================================
;; Relay with fakes
;; =============================================================================

(defrecord RecordingSource [queue listeners drains]
  ports/ISenseSource
  (listen! [_ k f] (swap! listeners assoc k f))
  (unlisten! [_ k] (swap! listeners dissoc k))
  (drain! [_ consumer _receptor]
    (swap! drains conj consumer)
    (let [[out] (swap-vals! queue (constantly []))] out)))

(defn- recording-source [] (->RecordingSource (atom []) (atom {}) (atom [])))

(defn- hear!
  "SOURCE hears SENSE: queue it and wake every listener, as sixth-sense does."
  [source s]
  (swap! (:queue source) conj s)
  (doseq [[_ f] @(:listeners source)] (f s)))

(defn- eventually
  ([pred] (eventually pred 5000))
  ([pred ms]
   (let [deadline (+ (System/currentTimeMillis) ms)]
     (loop []
       (let [v (pred)]
         (if (or v (> (System/currentTimeMillis) deadline)) v
             (do (Thread/sleep 10) (recur))))))))

(deftest relay-waits-for-a-loop-client-then-delivers-and-replays
  (let [src (recording-source)
        sent (atom [])
        client? (atom false)
        r (relay/start! {:source src :broadcast! #(swap! sent conj %)
                         :loop-client? #(deref client?)})]
    (try
      (hear! src (sense "s1" :sense/blocked))
      (Thread/sleep 100)
      (is (= [] @sent) "no loop-capable client: nothing sent")
      (is (= [] @(:drains src)) "and nothing drained, so the sense stays in sixth-sense")
      (reset! client? true)
      (relay/connected! r)
      (is (eventually #(= ["s1"] (map (fn [o] (get o "id")) @sent))))
      (hear! src (sense "s2" :sense/completed))
      (is (eventually #(= ["s1" "s2"] (map (fn [o] (get o "id")) @sent))))
      (is (= ["loop/steer" "loop/followup"] (map #(get % "op") @sent)))
      (is (true? (relay/ack! r "s1")))
      (is (nil? (relay/ack! r "s1")) "a second ack is a no-op")
      (reset! sent [])
      (relay/connected! r)
      (is (eventually #(= ["s2"] (map (fn [o] (get o "id")) @sent)))
          "a reconnect replays only what was never acked")
      (is (= {:senses :listening :pending 1 :sent 3 :acked 1 :ignored 0} (relay/status r)))
      (finally (relay/stop! r)))
    (is (empty? @(:listeners src)) "stop! unregisters the listener")))

(deftest relay-without-sixth-sense-is-idle
  (let [r (relay/start! {:source nil :broadcast! (fn [_] (throw (ex-info "never" {})))
                         :loop-client? (constantly true)})]
    (try
      (is (= 0 (relay/pump! r)))
      (is (= :absent (:senses (relay/status r))))
      (finally (relay/stop! r)))))

;; =============================================================================
;; Host end to end on loopback
;; =============================================================================

(def ^HttpClient client
  (-> (HttpClient/newBuilder) (.version HttpClient$Version/HTTP_1_1) (.build)))

(defn- temp-discovery []
  (str (Files/createTempDirectory "hive-dirge-sense" (make-array FileAttribute 0))
       "/run/hive-vessel/dirge.json"))

(defn- subscribe
  "Open the event stream; returns [line-queue close-fn]."
  [u]
  (let [q (LinkedBlockingQueue.)
        resp (.send client (-> (HttpRequest/newBuilder (URI. u)) (.timeout (Duration/ofSeconds 10)) (.build))
                    (HttpResponse$BodyHandlers/ofLines))
        stream (.body resp)
        ;; line by line: iterator-seq is chunked (1.12) and would hold lines back
        it (.iterator stream)
        reader (future (try (while (.hasNext it) (.put q (.next it)))
                            (catch Throwable _ nil)))]
    [q (fn [] (.close stream) (future-cancel reader))]))

(defn- next-op
  "The data of the next non-comment SSE event on Q, parsed, within 5 s."
  [^LinkedBlockingQueue q]
  (let [deadline (+ (System/currentTimeMillis) 5000)]
    (loop []
      (let [l (.poll q (max 1 (- deadline (System/currentTimeMillis))) TimeUnit/MILLISECONDS)]
        (cond (nil? l) nil
              (str/starts-with? l "data: ") (wire/read-json (subs l 6))
              :else (recur))))))

(defn- post [u body]
  (.statusCode (.send client (-> (HttpRequest/newBuilder (URI. u))
                                 (.timeout (Duration/ofSeconds 10))
                                 (.POST (HttpRequest$BodyPublishers/ofString body)) (.build))
                      (HttpResponse$BodyHandlers/ofString))))

(deftest host-relays-senses-to-a-loop-client-and-releases-them-on-ack
  (let [path (temp-discovery)
        src (recording-source)
        a (host/addon-ctor {:dirge/discovery-path path :dirge/sense-source src
                            :dirge/sense-policy {"completed" "interject"}})]
    (try
      (is (:success? (addon/initialize! a {})))
      (let [doc (wire/read-json (slurp path))
            url (fn [route & [extra]] (str (get doc "url") route "?token=" (get doc "token") extra))
            status (fn [] ((:dirge/senses (addon/hooks a))))]
        (hear! src (sense "s1" :sense/error :sense/text "exit 1"))
        (testing "a client without the loop feature drains nothing"
          (let [[_ close] (subscribe (url "/events" "&vessel=dirge&features=spans"))]
            (Thread/sleep 150)
            (is (= [] @(:drains src)))
            (close)))
        (testing "a loop client gets the backlog on connect"
          (let [[q close] (subscribe (url "/events" "&vessel=dirge&features=spans,loop"))
                op (next-op q)]
            (is (= "loop/steer" (get op "op")))
            (is (= "s1" (get op "id")))
            (is (str/includes? (get op "prompt") "ling-1 failed"))
            (hear! src (sense "s2" :sense/completed))
            (is (= ["loop/interject" "s2"] ((juxt #(get % "op") #(get % "id")) (next-op q)))
                "the configured policy applies")
            (is (= 202 (post (url "/reply") "{\"action\":\"ack\",\"target\":\"s1\"}")))
            (is (eventually #(= 1 (:pending (status)))))
            (close)))
        (testing "a reconnect replays what was not acked"
          (let [[q close] (subscribe (url "/events" "&features=loop"))]
            (is (= "s2" (get (next-op q) "id")))
            (close)))
        (is (= {:senses :listening :pending 1 :acked 1}
               (select-keys (:senses (:details (addon/health a))) [:senses :pending :acked]))))
      (finally
        (addon/shutdown! a)
        (io/delete-file path true)))
    (is (empty? @(:listeners src)))))
