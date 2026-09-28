(ns hive-dirge.economy.economy-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.protocol :as p]
            [hive-dsl.result :as r]
            [hive-dirge.economy.addon :as addon]
            [hive-dirge.economy.adapters.local :as local]
            [hive-dirge.economy.domain :as d]
            [hive-dirge.economy.pipeline.observe :as observe]
            [hive-dirge.economy.pipeline.retrieve :as retrieve]
            [hive-dirge.economy.ports :as ports]))

;; ---------------------------------------------------------------------------
;; domain

(deftest canonical-ignores-key-order
  (is (= (d/signature "read" {:b 2 :a {:y 1 :x [1 2]}})
         (d/signature "read" {:a {:x [1 2] :y 1} :b 2}))))

(deftest observation-shape
  (let [o (d/observation {:tool "bash" :args {:command "ls"} :result "abcdefgh1" :error? nil})]
    (is (= {:tool "bash" :signature "bash {:command \"ls\"}" :body "abcdefgh1" :tokens 3 :error? false} o))
    (is (= "{:a 1}" (:body (d/observation {:tool "t" :result {:a 1}}))))
    (is (= "" (:body (d/observation {:tool "t" :result nil}))))))

(deftest content-key-is-stable-hex
  (let [o (d/observation {:tool "t" :args {} :result "x"})]
    (is (re-matches #"[0-9a-f]{16}" (d/content-key o)))
    (is (= (d/content-key o) (d/content-key (d/observation {:tool "t" :args {} :result "x"}))))
    (is (not= (d/content-key o) (d/content-key (d/observation {:tool "t" :args {} :result "y"}))))
    (is (not= (d/content-key o) (d/content-key (d/observation {:tool "u" :args {} :result "x"}))))))

(deftest content-key-matches-the-cljrs-runtime
  (testing "value printed by the cljrs smoke run for the same observation"
    (is (= "09145c5e99232a80"
           (d/content-key (d/observation {:tool "grep" :args {:pattern "x" :path "src"}
                                          :result "hit-1\nhit-2\nhit-3"}))))))

(deftest mint-handle-extends-on-collision
  (let [full  "aaaaaaaabbbbbbbb"
        other "aaaaaaaacccccccc"]
    (is (= "aaaaaaaa" (d/mint-handle {} full)))
    (is (= "aaaaaaaa" (d/mint-handle {"aaaaaaaa" full} full)) "own handle is reused")
    (is (= "aaaaaaaabb" (d/mint-handle {"aaaaaaaa" other} full)))
    (let [taken (into {} (map (fn [n] [(subs full 0 n) other])) (range 8 17 2))]
      (is (= (str full "-1") (d/mint-handle taken full))))))

(deftest parse-handle-normalises
  (is (= "1a2b3c4d" (d/parse-handle "§1A2B3C4D")))
  (is (= "1a2b3c4d" (d/parse-handle " 1a2b3c4d ")))
  (is (= "1a2b3c4d5e-2" (d/parse-handle "1a2b3c4d5e-2")))
  (is (nil? (d/parse-handle "xyz")))
  (is (nil? (d/parse-handle "1a2b")))
  (is (nil? (d/parse-handle nil))))

(deftest slice-ranges
  (let [body "l1\nl2\nl3\nl4"]
    (is (= body (d/slice nil body)))
    (is (= "l2\nl3" (d/slice (d/parse-range {:start 2 :end 3}) body)))
    (is (= "l3\nl4" (d/slice (d/parse-range {:start "3"}) body)))
    (is (= "" (d/slice (d/parse-range {:start 9 :end 12}) body)))
    (is (= "l1" (d/slice (d/parse-range {:end 1}) body)))
    (is (= "1\nl" (d/slice (d/parse-range {:unit "chars" :start 2 :end 4}) body)))
    (is (= "" (d/slice (d/parse-range {:start 3 :end 1}) body)))))

(defspec slice-never-throws-and-is-a-substring 200
  (prop/for-all [body gen/string
                 s    (gen/one-of [(gen/return nil) gen/small-integer])
                 e    (gen/one-of [(gen/return nil) gen/small-integer])
                 unit (gen/elements [nil "lines" "chars"])]
    (let [out (d/slice (d/parse-range {:start s :end e :unit unit}) body)]
      (and (string? out) (str/includes? body out)))))

(defspec handle-is-a-deterministic-prefix 200
  (prop/for-all [tool gen/string-alphanumeric
                 body gen/string]
    (let [o    (d/observation {:tool tool :args {:q body} :result body})
          full (d/content-key o)
          h    (d/mint-handle {} full)]
      (and (= 8 (count h)) (str/starts-with? full h) (= h (d/parse-handle (d/cite h)))))))

;; ---------------------------------------------------------------------------
;; local adapter

(deftest local-log-put-fetch
  (let [log (local/make-log)
        o   (d/observation {:tool "read" :args {:path "a"} :result "one\ntwo\nthree"})
        h   (ports/put-observation! log o)]
    (is (= 8 (count h)))
    (is (= h (ports/put-observation! log o)) "same content, same handle")
    (is (= "one\ntwo\nthree" (ports/fetch log h nil)))
    (is (= "two" (ports/fetch log h {:unit :lines :start 2 :end 2})))
    (is (nil? (ports/fetch log "deadbeef" nil)))
    (is (= 1 (:observations (local/log-stats log))))))

(deftest local-log-spill-is-append-only
  (let [lines (atom [])
        log   (local/make-log #(swap! lines conj %))
        o1    (d/observation {:tool "t" :result "a"})
        o2    (d/observation {:tool "t" :result "b"})
        h1    (ports/put-observation! log o1)
        _     (ports/put-observation! log o1)
        h2    (ports/put-observation! log o2)]
    (is (= 2 (count @lines)))
    (is (= [h1 h2] (map (comp :handle read-string) @lines)))
    (is (= "a" (:body (read-string (first @lines)))))))

(deftest local-log-spill-failure-is-fail-open
  (let [log (local/make-log (fn [_] (throw (ex-info "disk full" {}))))
        h   (ports/put-observation! log (d/observation {:tool "t" :result "a"}))]
    (is (= "a" (ports/fetch log h nil)))
    (is (= 1 (:spill-errors (local/log-stats log))))))

(deftest local-log-spills-to-a-real-file
  (let [dir  (str (System/getProperty "java.io.tmpdir") "/econ-" (random-uuid))
        path (local/spill-path dir "sess")
        log  (local/make-log (local/file-spill (constantly path)))
        h    (ports/put-observation! log (d/observation {:tool "t" :result "x"}))]
    (try
      (is (= (str dir "/.dirge/economy/sess.edn") path))
      (is (= h (:handle (read-string (slurp path)))))
      (finally
        (io/delete-file path true)))))

;; ---------------------------------------------------------------------------
;; pipelines

(defn- env [] {:log (local/make-log) :stats (atom {})})

(deftest observe-stores-and-answers-nil
  (let [e (env)]
    (is (nil? (observe/after-tool-call e {:tool "bash" :args {:c 1} :result "out" :error? false})))
    (is (nil? (observe/after-tool-call e {:tool "context_retrieve" :args {} :result "x"})))
    (is (= {:observed 1} @(:stats e)))
    (is (= 1 (:observations (local/log-stats (:log e)))))))

(deftest observe-survives-a-throwing-log
  (let [e {:log (reify ports/IObservationLog
                  (put-observation! [_ _] (throw (ex-info "boom" {})))
                  (fetch [_ _ _] nil))
           :stats (atom {})}]
    (is (nil? (observe/after-tool-call e {:tool "t" :result "r"})))
    (is (= {:observe-errors 1} @(:stats e)))))

(deftest retrieve-hit-miss-and-counts
  (let [e (env)
        h (ports/put-observation! (:log e) (d/observation {:tool "t" :result "a\nb\nc"}))]
    (is (= {:text "a\nb\nc" :found? true} (retrieve/retrieve e {:handle (d/cite h)})))
    (is (= {:text "b" :found? true} (retrieve/retrieve e {:handle h :start 2 :end 2})))
    (let [miss (retrieve/retrieve e {:handle "§00000000"})]
      (is (false? (:found? miss)))
      (is (str/includes? (:text miss) "unknown handle §00000000")))
    (is (str/includes? (:text (retrieve/retrieve e {:handle "not a handle"})) "unknown handle"))
    (is (= {:retrieves 4 :hits 2 :misses 2} @(:stats e)))))

;; ---------------------------------------------------------------------------
;; addon

(def manifest-path "META-INF/hive-addons/hive-dirge-economy.edn")

(deftest manifest-reads-and-resolves
  (let [spec (boundary/parse-spec (slurp (io/resource manifest-path)))]
    (is (r/ok? spec) (pr-str spec))
    (let [m   (:ok spec)
          res (boundary/resolve-constructor m)]
      (is (= "hive.dirge.economy" (:addon/id m)))
      (is (= :resolved (:constructor/status res)) (pr-str res))
      (is (identical? addon/addon-ctor (:constructor res)))))
  (is (some #(= "hive.dirge.economy" (:addon/id %)) (:specs (boundary/discover-specs)))))

(deftest addon-end-to-end
  (let [a     (addon/make-addon {:cwd (constantly nil)})
        hooks (p/hooks a)
        tool  (:handler (first (p/tools a)))]
    (is (p/addon? a))
    (is (= :down (:status (p/health a))))
    (is (:success? (p/initialize! a {:addon/id "hive.dirge.economy"})))
    (is (contains? (p/capabilities a) :context/economy))
    (is (nil? ((:dirge/session-start hooks) {:session-id "s1" :cwd nil})))
    (is (nil? ((:dirge/after-tool-call hooks) {:tool "grep" :args {:p "x"} :result "hit-1\nhit-2"})))
    (let [h (d/mint-handle {} (d/content-key (d/observation {:tool "grep" :args {:p "x"} :result "hit-1\nhit-2"})))]
      (is (= {:content [{:type "text" :text "hit-2"}] :isError false}
             (tool {:handle (d/cite h) :start 2})))
      (is (true? (:isError (tool {:handle "§ffffffff"})))))
    (let [details (:details (p/health a))]
      (is (= 1 (:observations details)))
      (is (= 2 (:retrieves details)))
      (is (= 1 (:hits details))))))

(deftest addon-outside-dirge-is-safe
  (let [a (addon/addon-ctor {})]
    (p/initialize! a {})
    (is (nil? ((:dirge/after-tool-call (p/hooks a)) {:tool "t" :result "r"})))
    (is (= :ok (:status (p/health a))))))
