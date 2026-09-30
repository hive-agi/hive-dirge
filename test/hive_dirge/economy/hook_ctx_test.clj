(ns hive-dirge.economy.hook-ctx-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dirge.economy.adapters.local :as local]
            [hive-dirge.economy.domain :as d]
            [hive-dirge.economy.markdown :as md]
            [hive-dirge.economy.pipeline.compact :as compact]
            [hive-dirge.economy.pipeline.observe :as observe]
            [hive-dirge.economy.ports :as ports]
            [hive-dirge.economy.registry :as registry]))

(defn- env
  ([] (env (local/make-log)))
  ([log]
   {:log      log
    :stats    (atom {})
    :digests  (atom {})
    :config   {}
    :digestor (registry/select :digestor {})}))

(defn- ctx
  [span & {:as more}]
  (merge {:span span :tokens 5000 :reason "threshold" :session-id "s1"} more))

(defn- digest-of
  [e c]
  (some-> (compact/compact e c) :summary md/parse))

(defn- handles-in
  [text]
  (set (map second (re-seq md/handle-re text))))

;; ---------------------------------------------------------------------------
;; args-text

(deftest args-text-is-one-clipped-line
  (is (nil? (d/args-text nil)))
  (is (nil? (d/args-text {})))
  (is (nil? (d/args-text "{}")))
  (is (nil? (d/args-text "  ")))
  (is (= "{:command \"ls\"}" (d/args-text {:command "ls"})))
  (is (= "{\"command\":\"cat <<EOF x EOF\"}" (d/args-text "{\"command\":\"cat <<EOF\nx\nEOF\"}")))
  (let [long (apply str (repeat 300 "a"))]
    (is (= (str (subs long 0 d/args-max-chars) "…") (d/args-text long)))))

(defspec args-text-never-breaks-a-bullet 200
  (prop/for-all [args (gen/one-of [gen/string (gen/map gen/keyword gen/string) (gen/return nil)])]
    (let [s (d/args-text args)]
      (or (nil? s)
          (and (not (str/includes? s "\n"))
               (<= (count s) (inc d/args-max-chars))
               (not (str/blank? s)))))))

(deftest signature-args-reads-back-the-args
  (is (= "{:command \"ls\"}" (d/signature-args (d/observation {:tool "bash" :args {:command "ls"} :result "x"}))))
  (is (nil? (d/signature-args (d/observation {:tool "bash" :args nil :result "x"})))))

;; ---------------------------------------------------------------------------
;; after-tool-call ctx

(deftest collect-keeps-the-tool-use-id-when-sent
  (is (= "t1" (:tool-use-id (observe/collect {:tool "bash" :result "x" :tool-use-id "t1"}))))
  (is (= "t2" (:tool-use-id (observe/collect {:tool "bash" :result "x" :tool-call-id "t2"}))))
  (is (not (contains? (observe/collect {:tool "bash" :result "x"}) :tool-use-id))
      "an older dirge sends no id: the Observation is what it was"))

(deftest log-joins-every-id-to-its-content-handle
  (let [log (local/make-log)
        h1  (ports/put-observation! log (d/observation {:tool "t" :result "same" :tool-use-id "a"}))
        h2  (ports/put-observation! log (d/observation {:tool "t" :result "same" :tool-use-id "b"}))]
    (is (= h1 h2) "content-addressed: the id never changes the handle")
    (is (= h1 (ports/handle-by-id log "a")))
    (is (= h1 (ports/handle-by-id log "b")))
    (is (nil? (ports/handle-by-id log "zzz")))
    (is (nil? (ports/handle-by-id log nil)))))

(deftest spill-line-carries-the-id
  (let [lines (atom [])
        log   (local/make-log #(swap! lines conj %))]
    (ports/put-observation! log (d/observation {:tool "t" :result "a" :tool-use-id "t9"}))
    (is (= "t9" (:tool-use-id (read-string (first @lines)))))))

;; ---------------------------------------------------------------------------
;; compact joins

(deftest exact-id-join-beats-the-tool-body-join
  (testing "two calls with the same tool and body: only the id tells them apart"
    (let [e  (env)
          _  (observe/after-tool-call e {:tool "bash" :args {:command "make a"} :result "ok" :tool-use-id "t1"})
          _  (observe/after-tool-call e {:tool "bash" :args {:command "make b"} :result "ok" :tool-use-id "t2"})
          h2 (ports/handle-by-id (:log e) "t2")
          d  (digest-of e (ctx [{:role "user" :text "build b"}
                                {:role "tool" :tool "bash" :tool-use-id "t2" :text "ok"}]))]
      (is (not= h2 (ports/handle-of (:log e) "bash" "ok")) "the body join would pick t1")
      (is (= #{h2} (set (map :handle (:citations d)))))
      (is (str/includes? (first (:done d)) "ran bash {:command \"make b\"}")
          "args come from the log when the span has none"))))

(deftest span-args-name-the-command-in-done-lines
  (let [e (env)
        d (digest-of e (ctx [{:role "user" :text "run tests"}
                             {:role "assistant" :tool "bash" :tool-use-id "t1"
                              :args "{\"command\":\"make test\"}" :text ""}
                             {:role "tool" :tool "bash" :tool-use-id "t1"
                              :args "{\"command\":\"make test\"}" :text "ok 12 tests"}]))]
    (is (str/starts-with? (first (:done d)) "E1.1 ran bash {\"command\":\"make test\"}: 1 line"))))

(deftest older-dirge-falls-back-to-the-body-join
  (testing "no :tool-use-id in after-tool-call ctx, no :args or id in the span"
    (let [e (env)
          _ (observe/after-tool-call e {:tool "grep" :args {:p "x"} :result "hit"})
          h (ports/handle-of (:log e) "grep" "hit")
          d (digest-of e (ctx [{:role "user" :text "find x"}
                               {:role "tool" :tool "grep" :text "hit"}]))]
      (is (= [h] (map :handle (:citations d))))
      (is (str/includes? (first (:done d)) "ran grep {:p \"x\"}")))))

(deftest a-log-without-the-join-port-still-joins-by-body
  (let [inner (local/make-log)
        log   (reify
                ports/IObservationLog
                (put-observation! [_ o] (ports/put-observation! inner o))
                (fetch [_ h r] (ports/fetch inner h r))
                ports/IObservationIndex
                (handle-of [_ t b] (ports/handle-of inner t b)))
        e     (env log)
        _     (observe/after-tool-call e {:tool "bash" :args {:c 1} :result "out" :tool-use-id "t1"})
        h     (ports/handle-of inner "bash" "out")
        d     (digest-of e (ctx [{:role "user" :text "go"}
                                 {:role "tool" :tool "bash" :tool-use-id "t1" :text "out"}]))]
    (is (= [h] (map :handle (:citations d))))
    (is (str/includes? (first (:done d)) "ran bash: 1 line") "no args port: no args, no failure")
    (is (zero? (:digest-errors @(:stats e) 0)))))

;; ---------------------------------------------------------------------------
;; span carry-forward

(deftest a-system-summary-marker-in-the-span-is-the-prior
  (let [e  (env)
        s1 (:summary (compact/compact e (ctx [{:role "user" :text "Fix src/a.clj"}
                                              {:role "tool" :tool "bash" :text "FAIL one"}])))
        _  (reset! (:digests e) {})
        d2 (digest-of e (ctx [{:role "system" :text s1}
                              {:role "tool" :tool "bash" :text "ok"}
                              {:role "user" :text "next"}]))]
    (is (= 2 (:epoch d2)) "the span's prior carries the epoch without the session fallback")
    (is (every? (set (map :handle (:citations d2))) (handles-in s1)))
    (is (= "Fix src/a.clj" (:task d2)))
    (is (= 1 (:prior-from-span @(:stats e))))
    (is (nil? (:prior-from-session @(:stats e))))))

(deftest the-span-prior-wins-over-the-session-one
  (let [e  (env)
        s1 (:summary (compact/compact e (ctx [{:role "user" :text "task one"}
                                              {:role "tool" :tool "t" :text "r1"}])))
        _  (compact/compact e (ctx [{:role "user" :text "more"}
                                    {:role "tool" :tool "t" :text "r2"}]))
        d3 (digest-of e (ctx [{:role "system" :text s1}
                              {:role "user" :text "go on"}]))]
    (is (= 2 (:epoch d3)) "built on s1 (epoch 1), not on the session's epoch-2 digest")
    (is (= 1 (:prior-from-span @(:stats e))))
    (is (= 1 (:prior-from-session @(:stats e))) "only the marker-less middle fold used it")))

(deftest the-session-fallback-still-covers-a-span-without-markers
  (let [e (env)
        _ (compact/compact e (ctx [{:role "user" :text "task one"} {:role "tool" :tool "t" :text "r1"}]))
        d (digest-of e (ctx [{:role "user" :text "go on"} {:role "tool" :tool "t" :text "r2"}]))]
    (is (= 2 (:epoch d)))
    (is (= 1 (:prior-from-session @(:stats e))))))

(defspec any-id-joins-to-the-handle-that-retrieves-it 50
  (prop/for-all [id   gen/string-alphanumeric
                 body (gen/not-empty gen/string-alphanumeric)
                 args (gen/map gen/keyword gen/small-integer)]
    (let [e (env)
          _ (observe/after-tool-call e {:tool "t" :args args :result body :tool-use-id (str "id" id)})
          d (digest-of e (ctx [{:role "user" :text "q"}
                               {:role "tool" :tool "t" :tool-use-id (str "id" id) :text body}]))
          h (:handle (first (:citations d)))]
      (and (= h (ports/handle-by-id (:log e) (str "id" id)))
           (= body (ports/fetch (:log e) h nil))))))
