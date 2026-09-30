(ns hive-dirge.economy.watch-test
  "The economy's :dirge/event observers over payloads shaped as dirge
   projects them (docs/addons.md, Watching the run)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dirge.economy.adapters.local :as local]
            [hive-dirge.economy.domain :as d]
            [hive-dirge.economy.pipeline.watch :as watch]
            [hive-dirge.economy.ports :as ports]))

(defn- env []
  {:log (local/make-log) :stats (atom {}) :pending (atom {})})

(defn- handle-of [tool args result]
  (d/mint-handle {} (d/content-key (d/observation {:tool tool :args args :result result}))))

(deftest a-tool-call-and-its-result-become-one-observation
  (let [e (env)]
    (is (nil? (watch/on-event e {:event :tool-call :id "c1" :tool "grep" :args {:p "x"}})))
    (is (= {"c1" {:tool "grep" :args {:p "x"}}} @(:pending e)))
    (is (nil? (watch/on-event e {:event :tool-result :id "c1" :output "hit-1\nhit-2"})))
    (is (= {} @(:pending e)) "the pairing is consumed")
    (is (= "hit-1\nhit-2" (ports/fetch (:log e) (handle-of "grep" {:p "x"} "hit-1\nhit-2") nil))
        "same handle after-tool-call would have minted for the same call")
    (is (= {:events 2 :observed 1} @(:stats e)))))

(deftest string-event-names-are-heard-too
  (let [e (env)]
    (watch/on-event e {:event "tool-call" :id "c1" :tool "read" :args {:path "a"}})
    (watch/on-event e {:event "tool-result" :id "c1" :output "body"})
    (is (= 1 (:observed @(:stats e))))))

(deftest interleaved-calls-pair-by-id
  (let [e (env)]
    (watch/on-event e {:event :tool-call :id "a" :tool "read" :args {:path "a"}})
    (watch/on-event e {:event :tool-call :id "b" :tool "read" :args {:path "b"}})
    (watch/on-event e {:event :tool-result :id "b" :output "B"})
    (watch/on-event e {:event :tool-result :id "a" :output "A"})
    (is (= "A" (ports/fetch (:log e) (handle-of "read" {:path "a"} "A") nil)))
    (is (= "B" (ports/fetch (:log e) (handle-of "read" {:path "b"} "B") nil)))))

(deftest retrieval-results-are-not-logged
  (let [e (env)]
    (watch/on-event e {:event :tool-call :id "r" :tool "context_retrieve" :args {:handle "x"}})
    (watch/on-event e {:event :tool-result :id "r" :output "old body"})
    (is (nil? (:observed @(:stats e))))
    (is (= 0 (:observations (local/log-stats (:log e)))))))

(deftest unpaired-and-clipped-results-are-counted
  (let [e (env)]
    (watch/on-event e {:event :tool-result :id "zz" :output "orphan"})
    (watch/on-event e {:event :tool-call :id "c" :tool "read" :args {}})
    (watch/on-event e {:event :tool-result :id "c" :output "xxxx…[70000 more bytes]"})
    (is (= {:events 3 :observed 2 :observe-unpaired 1 :observe-clipped 1} @(:stats e)))))

(deftest pending-is-bounded
  (let [e (env)]
    (doseq [i (range (+ 10 watch/max-pending))]
      (watch/on-event e {:event :tool-call :id (str i) :tool "t" :args {}}))
    (is (<= (count @(:pending e)) watch/max-pending))))

(deftest turns-usage-runs-and-compactions-count
  (let [e (env)]
    (doseq [ev [{:event :turn-start :index 0}
                {:event :usage :input-tokens 10 :cached-input-tokens 2
                 :cache-creation-input-tokens 0 :output-tokens 5}
                {:event :turn-start :index 1}
                {:event :usage :input-tokens 7 :cached-input-tokens 1
                 :cache-creation-input-tokens 3 :output-tokens 4}
                {:event :compaction-started :tokens-before 900}
                {:event :context-compacted :session-id "s2" :tokens-before 900
                 :tokens-after 300 :summary "sum" :kind "prune-and-summary"}
                {:event :done :response "ok" :tokens 29 :cost 0.1}
                {:event :notice :content "ignored"}]]
      (is (nil? (watch/on-event e ev))))
    (is (= {:events 8
            :turns 2 :last-turn 1
            :usage {:input-tokens 17 :cached-input-tokens 3
                    :cache-creation-input-tokens 3 :output-tokens 9}
            :compaction-started 1 :last-compaction-started {:tokens-before 900}
            :compacted 1 :last-compacted {:tokens-before 900 :tokens-after 300
                                          :kind "prune-and-summary"}
            :runs 1 :last-run-tokens 29}
           @(:stats e)))))

(deftest a-throwing-log-never-reaches-dirge
  (let [e (assoc (env) :log (reify ports/IObservationLog
                              (put-observation! [_ _] (throw (ex-info "boom" {})))
                              (fetch [_ _ _] nil)))]
    (watch/on-event e {:event :tool-call :id "c" :tool "t" :args {}})
    (is (nil? (watch/on-event e {:event :tool-result :id "c" :output "r"})))
    (is (= 1 (:observe-errors @(:stats e))))))

(deftest a-broken-env-counts-a-watch-error
  (testing "pending that is not an atom throws inside the observer"
    (let [e (assoc (env) :pending :not-an-atom)]
      (is (nil? (watch/on-event e {:event :tool-call :id "c" :tool "t" :args {}})))
      (is (= 1 (:watch-errors @(:stats e)))))))

(def gen-event
  (gen/hash-map :event (gen/one-of [(gen/elements [:tool-call :tool-result :turn-start :usage
                                                   :done :compaction-started :context-compacted
                                                   "tool-call" "tool-result" "usage"])
                                    gen/any-equatable])
                :id gen/any-equatable
                :tool gen/any-equatable
                :args gen/any-equatable
                :output gen/any-equatable
                :index gen/any-equatable
                :input-tokens (gen/one-of [gen/small-integer gen/any-equatable])))

(defspec on-event-answers-nil-and-never-throws 200
  (prop/for-all [evs (gen/vector gen-event 0 12)]
    (let [e (env)]
      (every? nil? (map #(watch/on-event e %) evs)))))
