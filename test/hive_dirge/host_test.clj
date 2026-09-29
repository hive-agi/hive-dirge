(ns hive-dirge.host-test
  "hive.dirge.host against hive-vessel's real SSE executor on loopback.
   hive.olympus is replaced by a RECORDING IOlympusControl passed through
   config (:dirge/olympus), or by a stub IAddon injected by the real mounter;
   no with-redefs anywhere."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.mount :as mount]
            [hive-addon.mount.port :as mount-port]
            [hive-addon.protocol :as addon]
            [hive-dirge.host :as host]
            [hive-dirge.hive.addon :as hive-addon]
            [hive-dirge.lens.registry :as lens]
            [hive-dirge.host.boundary :as boundary]
            [hive-dirge.host.domain :as domain]
            [hive-dirge.host.ports :as ports]
            [hive-vessel.dialect.json :as json]
            [hive-vessel.executor.sse :as sse]
            [hive-vessel.wire :as wire])
  (:import (java.net URI)
           (java.time Duration)
           (java.net.http HttpClient HttpClient$Version HttpRequest
                          HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.nio.file Files LinkOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util.concurrent CountDownLatch LinkedBlockingQueue TimeUnit)))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defrecord RecordingOlympus [calls]
  ports/IOlympusControl
  (focus! [_ agent-id] (swap! calls conj [:focus agent-id]) :focused)
  (next-tab! [_] (swap! calls conj [:next-tab]) :next)
  (prev-tab! [_] (swap! calls conj [:prev-tab]) :prev)
  (refresh! [_] (swap! calls conj [:refresh]) :refreshed))

(defn- recording [] (->RecordingOlympus (atom [])))

(defn- temp-discovery []
  (str (Files/createTempDirectory "hive-dirge-test" (make-array FileAttribute 0))
       "/run/hive-vessel/dirge.json"))

(defn- perms-of [path]
  (PosixFilePermissions/toString
   (Files/getPosixFilePermissions (.toPath (io/file path)) (make-array LinkOption 0))))

(def ^HttpClient client
  (-> (HttpClient/newBuilder) (.version HttpClient$Version/HTTP_1_1) (.build)))

(defn- discovery [path] (wire/read-json (slurp path)))

(defn- url [doc route & [token]]
  (str (get doc "url") route "?token=" (or token (get doc "token"))))

(defn- request [u & {:keys [method body headers]}]
  (let [b (reduce (fn [b [k v]] (.header b k v)) (doto (HttpRequest/newBuilder (URI. u)) (.timeout (Duration/ofSeconds 10))) headers)
        b (if (= :post method)
            (.POST b (HttpRequest$BodyPublishers/ofString (or body "")))
            (.GET b))]
    (.send client (.build b) (HttpResponse$BodyHandlers/ofString))))

(defn- post-reply [doc body] (.statusCode (request (url doc "/reply") :method :post :body body)))

(defn- eventually
  "Poll PRED every 10 ms for up to MS (default 5000); its last value."
  ([pred] (eventually pred 5000))
  ([pred ms]
   (let [deadline (+ (System/currentTimeMillis) ms)]
     (loop []
       (let [v (pred)]
         (if (or v (> (System/currentTimeMillis) deadline))
           v
           (do (Thread/sleep 10) (recur))))))))

(defn- elapsed-ms [f]
  (let [t0 (System/nanoTime)
        v (f)]
    [v (/ (- (System/nanoTime) t0) 1e6)]))

(def ^:dynamic *subscriptions* nil)

(use-fixtures :each
  (fn [test-fn]
    (binding [*subscriptions* (atom [])]
      (try (test-fn)
           (finally
             (doseq [{:keys [stream reader]} @*subscriptions*]
               (.close ^java.util.stream.Stream stream)
               (future-cancel reader)))))))

(defn- open-events-url
  "Subscribe to a raw URL. Both the response stream and reader are closed by
   the per-test fixture, even when an assertion fails."
  [u]
  (let [q (LinkedBlockingQueue.)
        req (-> (HttpRequest/newBuilder (URI. u))
                (.timeout (Duration/ofSeconds 10)) (.build))
        resp (.send client req (HttpResponse$BodyHandlers/ofLines))
        stream (.body resp)
        reader (future
                 (try (doseq [l (iterator-seq (.iterator stream))] (.put q l))
                      (catch Throwable _ nil)))]
    (swap! *subscriptions* conj {:stream stream :reader reader})
    q))

(defn- open-events [doc]
  (open-events-url (url doc "/events")))

(defn- next-line [^LinkedBlockingQueue q] (.poll q 5 TimeUnit/SECONDS))

(defn- read-frame
  "Next complete SSE event, skipping comments, within five seconds TOTAL.
   Heartbeats must not reset the deadline when no event will ever arrive."
  [^LinkedBlockingQueue q]
  (let [deadline (+ (System/nanoTime) (.toNanos (Duration/ofSeconds 5)))]
    (loop [acc {}]
      (let [remaining (- deadline (System/nanoTime))
            l (when (pos? remaining) (.poll q remaining TimeUnit/NANOSECONDS))]
        (cond
          (nil? l) (when (seq acc) acc)
          (= "" l) (if (seq acc) acc (recur acc))
          (str/starts-with? l ":") (recur acc)
          :else (let [[k v] (str/split l #": ?" 2)]
                  (recur (assoc acc k v))))))))

(defmacro with-host [[sym config] & body]
  `(let [~sym (host/addon-ctor ~config)]
     (try
       (is (:success? (addon/initialize! ~sym {})))
       ~@body
       (finally (addon/shutdown! ~sym)))))

(def panel {:op :ui/show-panel :panel/id "olympus/tab-1"
            :doc {:doc/title "Olympus" :doc/blocks [{:block/type :para :text "No active agents"}]}})

;; =============================================================================
;; Pure domain
;; =============================================================================

(deftest discovery-doc-shape
  (let [d (domain/discovery-doc {:port 4321 :token "t0k" :pid 9})]
    (is (= "http://127.0.0.1:4321/vessel" (get d "url")))
    (is (= "t0k" (get d "token")))
    (is (= "dirge" (get d "vessel")))
    (is (= "json" (get d "dialect")))
    (is (= d (wire/read-json (domain/discovery-json {:port 4321 :token "t0k" :pid 9}))))
    (is (= "/run/user/1000/hive-vessel/dirge.json" (domain/discovery-path "/run/user/1000")))
    (is (= "/tmp/hive-vessel/dirge.json" (domain/discovery-path nil)))))

(deftest reply-parsing
  (is (= {:command :olympus/focus :agent-id "ling-1"}
         (domain/parse-reply "{\"action\":\"focus\",\"target\":\"ling-1\"}")))
  (is (= {:command :olympus/next-tab} (domain/parse-reply "{\"action\":\"next-tab\"}")))
  (is (= :reply/missing-target (:reply/error (domain/parse-reply "{\"action\":\"focus\"}"))))
  (is (= :reply/unknown-action (:reply/error (domain/parse-reply "{\"action\":\"rm -rf\"}"))))
  (is (= {:command :invoke
          :invoke  {"panel" "kanban" "verb" "open" "row" "t1" "payload" {}}}
         (domain/parse-reply "{\"action\":\"invoke\",\"panel\":\"kanban\",\"verb\":\"open\",\"row\":\"t1\"}")))
  (is (= {:command :invoke
          :invoke  {"panel" "swarm" "verb" "focus" "row" nil "payload" {"k" 1}}}
         (domain/parse-reply "{\"action\":\"invoke\",\"panel\":\"swarm\",\"verb\":\"focus\",\"payload\":{\"k\":1}}"))
      "payload defaults to {}, a missing row is nil")
  (is (= :reply/malformed-invoke
         (:reply/error (domain/parse-reply "{\"action\":\"invoke\",\"panel\":\"kanban\"}"))))
  (is (= :reply/malformed-invoke
         (:reply/error (domain/parse-reply "{\"action\":\"invoke\",\"panel\":\"\",\"verb\":\"open\"}"))))
  (is (= :reply/unparseable (:reply/error (domain/parse-reply "{not json"))))
  (is (= :reply/not-an-object (:reply/error (domain/parse-reply "[1]")))))

(deftest route-to-recording-port
  (let [o (recording)]
    (is (= {:routed :olympus/focus :result :focused}
           (ports/route! o {:command :olympus/focus :agent-id "a"})))
    (ports/route! o {:command :olympus/unfocus})
    (ports/route! o {:command :olympus/next-tab})
    (ports/route! o {:command :olympus/prev-tab})
    (ports/route! o {:command :olympus/refresh})
    (is (= {:reply/error :reply/unparseable} (ports/route! o {:reply/error :reply/unparseable})))
    (is (= [[:focus "a"] [:focus nil] [:next-tab] [:prev-tab] [:refresh]] @(:calls o))))
  (testing "an invoke routes through the InvokeRouter, never the olympus port"
    (let [o    (recording)
          runs (atom [])
          router (boundary/registry-invoke-router
                  {:registry-fn (constantly (lens/make-registry []))
                   :config      {:mount/dependencies {}}
                   :verb-fn     (fn [panel verb]
                                  (when (= ["kanban" "open"] [panel verb])
                                    (fn [invoke] (swap! runs conj invoke))))
                   :warn!       (fn [_ _])})]
      (is (= {:routed :invoke :result true}
             (ports/route! o router {:command :invoke
                                     :invoke {"panel" "kanban" "verb" "open"
                                              "row" "t1" "payload" {}}})))
      (is (= [{"panel" "kanban" "verb" "open" "row" "t1" "payload" {}}] @runs))
      (is (= {:routed :invoke :result false}
             (ports/route! o router {:command :invoke
                                     :invoke {"panel" "kanban" "verb" "nope"
                                              "row" nil "payload" {}}}))
          "an unknown verb is ignored with a warning, not a wire error")
      (is (= {:routed :invoke :result false}
             (ports/route! o router {:command :invoke
                                     :invoke {"panel" "ghost" "verb" "open"
                                              "row" nil "payload" {}}}))
          "an unknown panel is ignored with a warning")
      (is (= [] @(:calls o)) "olympus is untouched by invokes"))))

;; =============================================================================
;; Discovery file
;; =============================================================================

(deftest write-private-is-0600-in-0700
  (let [path (temp-discovery)]
    (boundary/write-private! path "{\"a\":1}")
    (is (= "rw-------" (perms-of path)))
    (is (= "rwx------" (perms-of (.getParent (io/file path)))))
    (is (= "{\"a\":1}" (slurp path)))
    (boundary/write-private! path "{\"a\":2}")
    (is (= "rw-------" (perms-of path)) "an overwrite stays 0600")
    (is (= ["dirge.json"] (vec (.list (.getParentFile (io/file path))))) "no temp file left")))

(deftest lifecycle-writes-and-removes-discovery
  (let [path (temp-discovery)
        a (host/addon-ctor {:dirge/discovery-path path :dirge/olympus (recording)})]
    (is (not (.exists (io/file path))) "construction is pure")
    (let [result (addon/initialize! a {})
          doc (discovery path)]
      (try
        (is (:success? result))
        (is (= "rw-------" (perms-of path)))
        (is (re-matches #"[0-9a-f]{32}" (get doc "token")))
        (is (= (str "http://127.0.0.1:" (get doc "port") "/vessel") (get doc "url")))
        (is (not (str/includes? (pr-str result) (get doc "token"))) "metadata never carries the token")
        (is (not (str/includes? (pr-str (addon/health a)) (get doc "token"))) "health never carries the token")
        (is (= :dirge (:vessel/id ((:vessel/target (addon/hooks a))))))
        (finally (addon/shutdown! a))))
    (is (not (.exists (io/file path))) "shutdown removes the discovery file")
    (is (= {} (addon/hooks a)))))

(deftest tokens-are-random
  (is (not= (boundary/new-token) (boundary/new-token))))

;; =============================================================================
;; Admission
;; =============================================================================

(deftest token-and-origin-admission
  (let [path (temp-discovery)]
    (with-host [a {:dirge/discovery-path path :dirge/olympus (recording)}]
      (let [doc (discovery path)]
        (is (= 200 (.statusCode (request (url doc "/health")))))
        (is (= 401 (.statusCode (request (url doc "/health" "wrong")))) "bad token")
        (is (= 401 (.statusCode (request (str (get doc "url") "/health")))) "no token")
        (is (= 403 (.statusCode (request (url doc "/health")
                                         :headers [["Origin" "http://127.0.0.1:3000"]])))
            "even a loopback browser Origin is refused")
        (is (= 403 (.statusCode (request (url doc "/reply") :method :post
                                         :body "{\"action\":\"refresh\"}"
                                         :headers [["Origin" "http://localhost"]])))
            "a browser POST never reaches olympus")
        (is (= 401 (.statusCode (request (url doc "/reply" "nope") :method :post
                                         :body "{\"action\":\"refresh\"}"))))
        (is (empty? @(:calls (:dirge/olympus (:seed a)))) "refused requests route nothing")))))

;; =============================================================================
;; Replies
;; =============================================================================

(deftest reply-answer-values
  (is (= 403 (domain/reply-admission {:origin-allowed? false :token-ok? false :method "POST"})))
  (is (= 401 (domain/reply-admission {:origin-allowed? true :token-ok? false :method "POST"})))
  (is (= 405 (domain/reply-admission {:origin-allowed? true :token-ok? true :method "GET"})))
  (is (nil? (domain/reply-admission {:origin-allowed? true :token-ok? true :method "POST"})))
  (is (= {:reply/accepted :olympus/refresh}
         (domain/reply-outcome {:command :olympus/refresh} true)))
  (is (= 202 (domain/reply-status (domain/reply-outcome {:command :olympus/refresh} true))))
  (is (= 503 (domain/reply-status (domain/reply-outcome {:command :olympus/refresh} false))))
  (is (= 400 (domain/reply-status (domain/reply-outcome {:reply/error :reply/unparseable} true)))
      "an error value is never accepted, whatever the queue said"))

(deftest replies-route-to-olympus
  (let [path (temp-discovery)
        o (recording)]
    (with-host [a {:dirge/discovery-path path :dirge/olympus o}]
      (let [doc (discovery path)]
        (is (= 202 (post-reply doc "{\"action\":\"focus\",\"target\":\"ling-7\"}")))
        (is (= 202 (post-reply doc "{\"action\":\"next-tab\"}")))
        (is (= 202 (post-reply doc "{\"action\":\"refresh\"}")))
        (is (= 202 (post-reply doc "{\"action\":\"unfocus\"}")))
        (is (= 400 (post-reply doc "garbage")) "a bad body is refused at once")
        (is (= 400 (post-reply doc "{\"action\":\"focus\"}")) "focus without a target")
        (is (= 400 (post-reply doc "{\"action\":\"rm -rf\"}")) "unknown action")
        (is (eventually #(= 4 (count @(:calls o)))))
        (is (= [[:focus "ling-7"] [:next-tab] [:refresh] [:focus nil]] @(:calls o)))
        (is (eventually #(= 7 (count ((:dirge/replies (addon/hooks a)))))))
        (is (= 3 (count (filter :reply/error ((:dirge/replies (addon/hooks a)))))))))))

(deftest reply-refusals-are-synchronous
  (let [path (temp-discovery)
        o (recording)]
    (with-host [_ {:dirge/discovery-path path :dirge/olympus o}]
      (let [doc (discovery path)]
        (is (= 401 (.statusCode (request (url doc "/reply" "nope") :method :post
                                         :body "{\"action\":\"refresh\"}"))))
        (is (= 401 (.statusCode (request (str (get doc "url") "/reply") :method :post
                                         :body "{\"action\":\"refresh\"}")))
            "no token")
        (is (= 403 (.statusCode (request (url doc "/reply") :method :post
                                         :body "{\"action\":\"refresh\"}"
                                         :headers [["Origin" "http://127.0.0.1"]]))))
        (is (= 405 (.statusCode (request (url doc "/reply")))) "GET /reply")
        (is (= 413 (post-reply doc (apply str (repeat (inc domain/max-reply-bytes) "x")))))
        (Thread/sleep 100)
        (is (empty? @(:calls o)) "no refusal reaches olympus")))))

;; A queue stub: records submissions and never runs them, so the answer can
;; only have come from the handler, not from olympus.
(defrecord StubQueue [submitted accept?]
  ports/IActionQueue
  (submit! [_ command] (swap! submitted conj command) accept?)
  (close! [_] nil))

(deftest reply-answers-from-the-queue-port
  (let [path (temp-discovery)
        o (recording)
        submitted (atom [])]
    (with-host [_ {:dirge/discovery-path path :dirge/olympus o
                   :dirge/action-queue (fn [_run] (->StubQueue submitted true))}]
      (let [doc (discovery path)]
        (is (= 202 (post-reply doc "{\"action\":\"prev-tab\"}")))
        (is (= 400 (post-reply doc "{nope")))
        (is (= [{:command :olympus/prev-tab}] @submitted) "only valid commands are offered")
        (is (empty? @(:calls o)) "the handler itself never calls olympus")))
    (let [full-path (temp-discovery)]
      (with-host [_ {:dirge/discovery-path full-path :dirge/olympus o
                     :dirge/action-queue (fn [_run] (->StubQueue (atom []) false))}]
        (is (= 503 (post-reply (discovery full-path) "{\"action\":\"refresh\"}"))
            "a full queue is answered 503")))))

(declare run-slow)

;; Slow olympus: every action takes DELAY-MS and logs its start and end.
(defrecord SlowOlympus [delay-ms log started]
  ports/IOlympusControl
  (focus! [this agent-id] (run-slow this [:focus agent-id]))
  (next-tab! [this] (run-slow this [:next-tab]))
  (prev-tab! [this] (run-slow this [:prev-tab]))
  (refresh! [this] (run-slow this [:refresh])))

(defn- run-slow [{:keys [delay-ms log ^CountDownLatch started]} call]
  (swap! log conj [:start call])
  (.countDown started)
  (Thread/sleep (long delay-ms))
  (swap! log conj [:end call])
  call)

(deftest reply-returns-before-a-slow-render-and-actions-stay-ordered
  (let [path (temp-discovery)
        log (atom [])
        o (->SlowOlympus 800 log (CountDownLatch. 1))]
    (with-host [_ {:dirge/discovery-path path :dirge/olympus o}]
      (let [doc (discovery path)
            bodies ["{\"action\":\"next-tab\"}"
                    "{\"action\":\"prev-tab\"}"
                    "{\"action\":\"focus\",\"target\":\"ling-3\"}"
                    "{\"action\":\"refresh\"}"]
            answers (mapv (fn [b] (elapsed-ms #(post-reply doc b))) bodies)]
        (is (= [202 202 202 202] (mapv first answers)))
        (is (every? #(< (second %) 500) answers)
            (str "each reply answers well under one 800 ms action: " (mapv second answers)))
        (is (.await ^CountDownLatch (:started o) 2 TimeUnit/SECONDS) "the worker picked up the first action")
        (is (not-any? #(= :end (first %)) @log) "every answer came before the first render finished")
        (is (eventually #(= 8 (count @log)) 6000))
        (is (= (mapcat (fn [c] [[:start c] [:end c]])
                       [[:next-tab] [:prev-tab] [:focus "ling-3"] [:refresh]])
               @log)
            "one at a time, in arrival order")))))

(deftest single-worker-queue-keeps-submission-order
  (let [ran (atom [])
        q (boundary/single-worker-queue {:capacity 1000
                                         :run-command (fn [c] (Thread/sleep 1) (swap! ran conj c))})]
    (try
      (is (every? true? (mapv #(ports/submit! q %) (range 200))))
      (is (eventually #(= 200 (count @ran))))
      (is (= (range 200) @ran))
      (finally (ports/close! q)))
    (is (false? (ports/submit! q :late)) "a closed queue refuses")))

(deftest single-worker-queue-refuses-when-full
  (let [gate (CountDownLatch. 1)
        q (boundary/single-worker-queue {:capacity 2 :run-command (fn [_] (.await gate))})]
    (try
      (is (true? (ports/submit! q 1)) "taken by the worker")
      (Thread/sleep 50)
      (is (true? (ports/submit! q 2)))
      (is (true? (ports/submit! q 3)))
      (is (false? (ports/submit! q 4)) "capacity 2 waiting")
      (finally (.countDown gate) (ports/close! q)))))

(defrecord StubAddon [id hook-map]
  addon/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] #{:olympus})
  (initialize! [_ _] {:success? true})
  (shutdown! [_] nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] hook-map))

(def olympus-calls (atom []))
(def seat (atom {}))

(defn olympus-stub-ctor [_]
  (->StubAddon "hive.olympus"
               {:olympus/register-presenter! (fn [id target] (swap! seat assoc id target) id)
                :olympus/unregister-presenter! (fn [id] (swap! seat dissoc id) id)
                :olympus/focus! (fn [id] (swap! olympus-calls conj [:focus id]))
                :olympus/next-tab! (fn [] (swap! olympus-calls conj [:next-tab]))
                :olympus/prev-tab! (fn [] (swap! olympus-calls conj [:prev-tab]))
                :olympus/refresh! (fn [] (swap! olympus-calls conj [:refresh]))}))

(defn- manifest [file]
  (some-> (io/resource (str "META-INF/hive-addons/" file)) slurp edn/read-string))

(deftest manifests-shape
  (let [h (manifest "hive-dirge-host.edn")
        o (manifest "hive-olympus-dirge.edn")]
    (is (= "hive.dirge.host" (:addon/id h)))
    (is (= "hive-dirge.host" (:addon/init-ns h)))
    (is (= :foss (:addon/trust-class h)))
    (is (= "hive-olympus.harness" (:addon/init-ns o)))
    (is (= "hive.dirge.host" (get-in o [:addon/config :olympus/host])))
    (is (= #{"hive.olympus" "hive.dirge.host"} (:addon/dependencies o)))
    (let [ids (set (map :addon/id (:specs (mount/discover-specs))))]
      (is (contains? ids "hive.dirge.host"))
      (is (contains? ids "hive.olympus.dirge")))))

(deftest mounted-end-to-end
  (reset! olympus-calls [])
  (reset! seat {})
  (let [path (temp-discovery)
        specs [(update (manifest "hive-dirge-host.edn") :addon/config assoc
                        :dirge/discovery-path path
                        ;; the JDK client surfaces a chunk only once more bytes follow
                        :dirge/heartbeat-ms 100)
               (manifest "hive-olympus-dirge.edn")
               {:addon/id "hive.olympus" :addon/type :native
                :addon/init-ns "hive-dirge.host-test" :addon/init-fn "olympus-stub-ctor"
                :addon/capabilities #{:olympus}}]
        host (mount/atom-mount-host)
        report (mount/mount! (mount/solve specs) host {:license-gate (constantly nil)})]
    (try
      (is (:ok? report) (pr-str (:mounted report)))
      (is (= ["hive.olympus" "hive.dirge.host" "hive.olympus.dirge"] (:order report)))
      (is (= "hive.olympus.dirge" (addon/addon-id (mount-port/registered host "hive.olympus.dirge"))))
      (let [doc (discovery path)
            q (open-events doc)]
        (testing "SSE stream opens with a retry frame"
          (is (= "retry: 2000" (next-line q))))
        (testing "the harness presenter delivers olympus panels as SSE frames"
          (let [present (get @seat "hive.dirge.host")]
            (is (fn? present) "the harness registered under the host id")
            (Thread/sleep 200)
            (is (contains? (present [panel]) :ok))
            (let [frame (read-frame q)
                  data (wire/read-json (get frame "data"))]
              (is (= "vessel" (get frame "event")))
              (is (= "1" (get frame "id")))
              (is (= "ui/show-panel" (get data "op")))
              (is (= "olympus/tab-1" (get data "panel/id")))
              (is (vector? (get data "lines")) "rendered lines for a simple client")
              (is (= (sse/sse-frame 1 {"op" "x"}) "id: 1\nevent: vessel\ndata: {\"op\":\"x\"}\n\n")
                  "hive-vessel's frame format: id, event, one data line, blank line"))))
        (testing "a late subscriber gets the retained panel replayed"
          (let [q2 (open-events doc)]
            (is (= "retry: 2000" (next-line q2)))
            (is (= "olympus/tab-1" (get (wire/read-json (get (read-frame q2) "data")) "panel/id")))))
        (testing "replies reach the injected hive.olympus hooks"
          (is (= 202 (post-reply doc "{\"action\":\"focus\",\"target\":\"ling-2\"}")))
          (is (= 202 (post-reply doc "{\"action\":\"prev-tab\"}")))
          (is (eventually #(= 2 (count @olympus-calls))))
          (is (= [[:focus "ling-2"] [:prev-tab]] @olympus-calls))))
      (finally
        (mount/teardown! host (:order report))
        (doseq [id (:order report)]
          (when-let [a (mount-port/registered host id)]
            (try (addon/shutdown! a) (catch Throwable _ nil))))))))

(deftest heartbeat-is-a-ping-comment
  (let [path (temp-discovery)]
    (with-host [_ {:dirge/discovery-path path :dirge/olympus (recording) :dirge/heartbeat-ms 100}]
      (let [q (open-events (discovery path))]
        (is (= "retry: 2000" (next-line q)))
        (is (= "" (next-line q)))
        (is (= ": ping" (next-line q)))))))


(def lens-calls (atom []))

(defn lens-stub-ctor [_]
  (hive-addon/make-addon
   {:mcp-call (fn [_server _tool _args]
                {:content [{:type "text" :text "ROWS"}]})
    :json-parse (fn [_] [{:id "task-42" :title "Task" :status "todo"
                           :priority "high" :project "proj"}])
    :panel! (fn [op] (swap! lens-calls conj op) true)
    :log! (fn [level msg] (swap! lens-calls conj [level msg]))
    :cwd (constantly "/w/proj")}))

(defn- handshake-bridge?
  "True when the hive-vessel bridge records per-client features (branch
   lens-c3-features / 0.1.13+). deps.edn pins 0.1.12, which lacks it, so the
   tests probe which side they run against and assert the matching contract."
  []
  (boolean (resolve 'hive-vessel.executor.sse/client-features)))

(def feature-panel
  "A lens-style show-panel: title, plain rows, dirge chords and a cursor."
  {:op :ui/show-panel
   :panel/id "kanban"
   :doc {:doc/title "Kanban" :doc/blocks [{:block/type :para :text "No active tasks"}]}
   :panel/rows [{:text "task-42" :face :row :id "task-42" :payload {:task 42}}]
   :keys {"enter" {"invoke" "open"}}
   :cursor true})

(deftest mounted-kanban-lens-wire
  (reset! lens-calls [])
  (let [path (temp-discovery)
        specs [(update (manifest "hive-dirge-host.edn") :addon/config assoc
                       :dirge/discovery-path path :dirge/heartbeat-ms 100)
               {:addon/id "hive.dirge" :addon/type :native
                :addon/init-ns "hive-dirge.host-test" :addon/init-fn "lens-stub-ctor"
                :addon/capabilities #{:dirge/lenses}}]
        mounted (mount/atom-mount-host)
        report (mount/mount! (mount/solve specs) mounted {:license-gate (constantly nil)})]
    (try
      (is (:ok? report) (pr-str report))
      (is (= ["hive.dirge" "hive.dirge.host"] (:order report)))
      (let [discovered (discovery path)
            caps (get discovered "capabilities")
            q (open-events-url (url discovered "/events"
                                       (str (get discovered "token")
                                            "&vessel=dirge&features=spans,keys,cursor")))
            addon-instance (mount-port/registered mounted "hive.dirge")
            dirge-host (mount-port/registered mounted "hive.dirge.host")]
        (is (= 1 (get caps "version")))
        (is (some #{"invoke"} (get caps "replies")))
        (is (= ["focus" "open"] (get caps "invokes")))
        (is (= {"invoke" "open"} (get-in caps ["keys" "enter"])))
        (is (= "retry: 2000" (next-line q)))
        ((get-in (addon/hooks addon-instance) [:dirge/commands "hive" :handler])
         {:argv ["kanban"] :cwd "/w/proj"})
        (let [op (first @lens-calls)
              dispatch! (:vessel/dispatch! (addon/hooks dirge-host))]
          (is (= "kanban" (:panel/id op)))
          (is (contains? (dispatch! op) :ok))
          (let [frame (fn []
                        (when-let [f (read-frame q)]
                          (when-let [d (get f "data")]
                            (when (= "ui/show-panel" (get (wire/read-json d) "op"))
                              f))))
                data (wire/read-json (get (eventually frame) "data"))]
            (is (= "kanban" (get data "panel/id")))
            (is (= (when (handshake-bridge?) true) (get data "cursor")))
            (is (= (when (handshake-bridge?) {"invoke" "open"})
                   (get-in data ["keys" "enter"])))
            (is (nil? (get data "spans"))
                "span rows ride inside \"lines\", never a top-level \"spans\"")
            (if (handshake-bridge?)
              (do (is (some #(and (= "task-42" (get % "id"))
                                  (= {"task" 42} (get % "payload")))
                            (get data "lines"))
                      "lens rows pass through with their ids and payloads")
                  (is (some #(and (nil? (get % "spans"))
                                  (= "task-42" (get % "text")))
                            (get data "lines"))
                      "span rows flatten to plain {text, face, id} lines"))
              (do (is (every? #(nil? (get % "spans")) (get data "lines"))
                      "pre-handshake hive-vessel renders plain lines only")
                  (is (some #(= "task-42" (get % "id")) (get data "lines"))
                      "row ids survive on plain lines")))))
        (is (= 202 (post-reply discovered
                               "{\"action\":\"invoke\",\"panel\":\"kanban\",\"verb\":\"open\",\"row\":\"task-42\",\"payload\":{}}")))
        (is (eventually #(some #{[:info "kanban open task-42"]} @lens-calls)))
        (is (= 202 (post-reply discovered "{\"action\":\"next-tab\"}"))))
      (finally
        (mount/teardown! mounted (:order report))))))

;; =============================================================================
;; Lens C3 handshake: :vessel/features and the feature-gated panel feed
;; =============================================================================

(deftest feature-set-version-is-1
  (is (= 1 domain/feature-set-version)))

(deftest panel-message-degrades-to-plain-lines-without-features
  (let [m (host/panel-message #{} feature-panel)]
    (is (= "Kanban" (get-in m ["lines" 0 "text"])))
    (is (= {"task" 42} (get-in m ["lines" 1 "payload"])))
    (is (= "task-42" (get-in m ["lines" 1 "id"]))
        "row ids survive on plain lines even with no client features")
    (is (nil? (get m "spans")))
    (is (nil? (get m "keys")))
    (is (nil? (get m "cursor"))))
  (is (= (json/show-panel-message {:op :ui/show-panel
                                   :panel/id "p"
                                   :doc {:doc/title "T" :doc/blocks []}})
         (host/panel-message nil {:op :ui/show-panel
                                  :panel/id "p"
                                  :doc {:doc/title "T" :doc/blocks []}}))
      "a doc-only panel with no client degrades to the plain dialect message"))

(deftest panel-message-upgrades-with-advertised-features
  (let [m (host/panel-message #{:spans :keys :cursor} feature-panel)]
    (is (nil? (get m "spans"))
        "dirge reads span rows inside \"lines\", never a top-level \"spans\"")
    (is (= {"invoke" "open"} (get-in m ["keys" "enter"])))
    (is (= true (get m "cursor"))))
  (testing "two-arity vessel renderer puts doc spans inside lines when available"
    (let [op {:op :ui/show-panel :panel/id "doc"
              :doc {:doc/title "Title" :doc/blocks [{:block/type :para :text "Body"}]}}
          message (host/panel-message #{:spans} op)
          two-arity? (some #{2} (:arglists (meta (resolve 'hive-vessel.dialect.json/show-panel-message))))]
      (is (nil? (get message "spans")))
      (is (= (boolean two-arity?) (boolean (some #(get % "spans") (get message "lines")))))))
  (testing "an unadvertised feature never leaks"
    (is (nil? (get (host/panel-message #{:keys} feature-panel) "spans")))
    (is (nil? (get (host/panel-message #{:cursor} feature-panel) "keys")))
    (is (nil? (get (host/panel-message #{} feature-panel) "cursor"))))
  (testing "an op without chords or cursor carries no keys/cursor fields"
    (let [m (host/panel-message #{:keys :cursor}
                                {:op :ui/show-panel :panel/id "p"
                                 :doc {:doc/title "T" :doc/blocks []}})]
      (is (nil? (get m "keys")))
      (is (nil? (get m "cursor"))))))

(deftest target-features-start-empty-and-follow-subscriptions
  (let [path (temp-discovery)]
    (with-host [a {:dirge/discovery-path path :dirge/olympus (recording)}]
      (let [target ((:vessel/target (addon/hooks a)))]
        (is (= :dirge (:vessel/id target)))
        (is (= #{} (:vessel/features target))
            "no dirge client has subscribed yet"))
      (let [doc (discovery path)
            sub (fn [] (open-events-url (url doc "/events"
                                             (str (get doc "token")
                                                  "&vessel=dirge&features=spans,keys,cursor,open-file"))))
            target-features (fn [] (:vessel/features ((:vessel/target (addon/hooks a)))))]
        (sub)
        (if (handshake-bridge?)
          (is (eventually
               (fn []
                 (= #{:spans :keys :cursor :open-file} (target-features))))
              "the bridge records the parsed features per client; the target answers them")
          (is (eventually
               (fn [] (= #{} (target-features))))
              "pre-handshake hive-vessel: the resolve fallback degrades to #{}"))))))

(deftest show-panel-feed-degrades-without-features-and-upgrades-with-them
  (let [path (temp-discovery)]
    (with-host [a {:dirge/discovery-path path :dirge/olympus (recording)
                   :dirge/heartbeat-ms 100}]
      (let [doc (discovery path)
            plain-q (open-events doc)
            rich-q (open-events-url (url doc "/events"
                                         (str (get doc "token")
                                              "&vessel=dirge&features=spans,keys,cursor")))]
        (is (= "retry: 2000" (next-line plain-q)))
        (is (= "retry: 2000" (next-line rich-q)))
        (let [dispatch! (:vessel/dispatch! (addon/hooks a))]
          (is (contains? (dispatch! feature-panel) :ok))
          (let [message-when (fn [pred q]
                               (fn []
                                 (when-let [frame (read-frame q)]
                                   (when-let [m (get frame "data")]
                                     (let [parsed (wire/read-json m)]
                                       (when (pred parsed) parsed))))))
                plain? (fn [m] (and (nil? (get m "keys"))
                                    (nil? (get m "cursor"))
                                    (nil? (get m "spans"))))
                rich? (fn [m] (and (= {"invoke" "open"} (get-in m ["keys" "enter"]))
                                   (= true (get m "cursor"))
                                   (nil? (get m "spans"))
                                   (some #(and (= "task-42" (get % "id"))
                                               (= {"task" 42} (get % "payload")))
                                         (get m "lines"))
                                   (some #(= "task-42" (get % "text"))
                                         (get m "lines"))))]
            (is (eventually (message-when plain? plain-q) 5000)
                "the unfeatured client gets plain lines only")
            (if (handshake-bridge?)
              (is (eventually (message-when rich? rich-q) 5000)
                  "a client that advertised spans/keys/cursor gets the upgraded feed")
              (is (eventually (message-when plain? rich-q) 5000)
                  "pre-handshake hive-vessel records no features, so the feed stays plain"))))))))
