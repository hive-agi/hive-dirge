(ns hive-dirge.ling.progress-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.protocol :as addon]
            [hive-dirge.ling.acp :as acp]
            [hive-dirge.ling.addon :as ling-addon]
            [hive-dirge.ling.domain :as d]
            [hive-dirge.ling.fake-agent :as fa]
            [hive-dirge.ling.progress :as pg]
            [hive-spi.addon.headless :as h]))

;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Recording sink over a manual clock and scheduler
;; =============================================================================

(defn- harness
  ([] (harness {}))
  ([{:keys [window-ms emit!] :or {window-ms 100}}]
   (let [clock (atom 0)
         tasks (atom [])
         seq-no (atom 0)
         emitted (atom [])
         sink (pg/progress-sink
               {:window-ms window-ms
                :clock (fn [] @clock)
                :schedule! (fn [delay-ms f]
                             (swap! tasks conj [(+ @clock delay-ms) (swap! seq-no inc) f]))
                :emit! (or emit! (fn [id u] (swap! emitted conj [@clock id u])))})]
     {:sink sink :clock clock :tasks tasks :emitted emitted :window-ms window-ms})))

(defn- advance!
  "Moves the clock to T, running every task due by then in due order."
  [{:keys [clock tasks]} t]
  (loop []
    (when-let [[due _ f :as task] (first (sort (filter #(<= (first %) t) @tasks)))]
      (swap! tasks (fn [ts] (vec (remove #(= % task) ts))))
      (reset! clock due)
      (f)
      (recur)))
  (reset! clock t))

(defn- feed! [hx id event] (pg/on-event! (:sink hx) id event))

(defn- plan [done total]
  (let [entries (vec (concat (repeat done {:content "x" :priority :medium :status :completed})
                             (repeat (- total done) {:content "y" :priority :medium :status :pending})))]
    {:event :ling/plan :entries entries :progress (d/plan-progress entries)}))

(defn- turn-end [cost]
  {:event :ling/turn-end :stop-reason :end-turn :usage {:cost-usd cost}})

(defn- progress-of [done total] {:ling/progress {:done done :total total}})

;; =============================================================================
;; Pure
;; =============================================================================

(deftest slave-update-reports-only-what-is-known
  (is (= {} (pg/slave-update d/empty-summary)))
  (is (= (progress-of 1 3) (pg/slave-update (d/summarize [(plan 1 3)]))))
  (is (= {:ling/cost-usd 0.5} (pg/slave-update (d/summarize [(turn-end 0.5)]))))
  (is (= (assoc (progress-of 2 2) :ling/cost-usd 0.75)
         (pg/slave-update (d/summarize [(plan 2 2) (turn-end 0.25) (turn-end 0.5)])))))

(deftest observe-sends-when-the-window-is-open-and-holds-otherwise
  (let [{l1 :ling e1 :emit} (pg/observe pg/empty-ling (plan 0 2) 1000 500)
        {l2 :ling e2 :emit f2 :flush-at} (pg/observe l1 (plan 1 2) 1100 500)
        {l3 :ling e3 :emit f3 :flush-at} (pg/observe l2 (plan 2 2) 1200 500)]
    (is (= (progress-of 0 2) e1))
    (is (nil? e2))
    (is (= 1500 f2))
    (is (nil? e3))
    (is (nil? f3) "one flush per window, not one per held change")
    (is (= (progress-of 2 2) (:pending l3)))
    (is (= {:emit (progress-of 2 2)} (select-keys (pg/flush-ling l3 1500 500) [:emit])))
    (is (= 1500 (:flush-at (pg/flush-ling l3 1400 500))) "an early flush re-arms")))

;; =============================================================================
;; Recording sink
;; =============================================================================

(deftest first-change-is-sent-at-once
  (let [hx (harness)]
    (feed! hx "l1" (plan 0 3))
    (is (= [[0 "l1" (progress-of 0 3)]] @(:emitted hx)))))

(deftest changes-inside-a-window-coalesce-to-the-last-value
  (let [hx (harness)]
    (feed! hx "l1" (plan 0 3))
    (advance! hx 10) (feed! hx "l1" (plan 1 3))
    (advance! hx 20) (feed! hx "l1" (plan 2 3))
    (advance! hx 30) (feed! hx "l1" (turn-end 0.1))
    (is (= 1 (count @(:emitted hx))))
    (advance! hx 100)
    (is (= [[0 "l1" (progress-of 0 3)]
            [100 "l1" (assoc (progress-of 2 3) :ling/cost-usd 0.1)]]
           @(:emitted hx)))))

(deftest unchanged-values-are-not-resent
  (let [hx (harness)]
    (feed! hx "l1" (plan 1 3))
    (feed! hx "l1" {:event :ling/text :text "hi"})
    (feed! hx "l1" (plan 1 3))
    (testing "a value that returns to the one sent inside the window sends nothing"
      (advance! hx 10) (feed! hx "l1" (plan 2 3))
      (advance! hx 20) (feed! hx "l1" (plan 1 3))
      (advance! hx 1000))
    (is (= [[0 "l1" (progress-of 1 3)]] @(:emitted hx)))))

(deftest events-without-progress-or-cost-send-nothing
  (let [hx (harness)]
    (doseq [e [{:event :ling/text :text "a"} {:event :ling/tool-call :tool-id "t"}
               {:event :ling/usage :usage {:context-used 3}} (plan 0 0)]]
      (feed! hx "l1" e))
    (advance! hx 1000)
    (is (= [] @(:emitted hx)))))

(deftest lings-have-independent-windows
  (let [hx (harness)]
    (feed! hx "a" (plan 0 1))
    (feed! hx "b" (plan 0 2))
    (advance! hx 10) (feed! hx "a" (plan 1 1))
    (is (= [[0 "a" (progress-of 0 1)] [0 "b" (progress-of 0 2)]] @(:emitted hx)))
    (advance! hx 100)
    (is (= [100 "a" (progress-of 1 1)] (last @(:emitted hx))))))

(deftest close-sends-held-values-and-forgets-lings
  (let [hx (harness)]
    (feed! hx "l1" (plan 0 2))
    (advance! hx 10) (feed! hx "l1" (plan 1 2))
    (pg/close! (:sink hx))
    (is (= [[0 "l1" (progress-of 0 2)] [10 "l1" (progress-of 1 2)]] @(:emitted hx)))
    (is (= {} @(:lings (:sink hx))))))

(deftest a-closed-ling-is-forgotten-once-flushed
  (let [hx (harness)]
    (feed! hx "l1" (plan 0 2))
    (advance! hx 10) (feed! hx "l1" (plan 2 2))
    (feed! hx "l1" {:event :ling/closed})
    (is (contains? @(:lings (:sink hx)) "l1") "held value still owed")
    (advance! hx 100)
    (is (= [100 "l1" (progress-of 2 2)] (last @(:emitted hx))))
    (is (not (contains? @(:lings (:sink hx)) "l1")))))

(deftest emit-failures-are-recorded-not-thrown
  (let [hx (harness {:emit! (fn [_ _] (throw (ex-info "slave gone" {})))})]
    (feed! hx "l1" (plan 0 1))
    (is (= [{:ling-id "l1" :update (progress-of 0 1) :error "slave gone"}]
           (pg/errors (:sink hx))))))

;; =============================================================================
;; Properties
;; =============================================================================

(def ^:private gen-event
  (gen/one-of
   [(gen/bind (gen/choose 1 6)
              (fn [total] (gen/fmap #(plan % total) (gen/choose 0 total))))
    (gen/fmap #(turn-end (/ % 100.0)) (gen/choose 0 300))
    (gen/return {:event :ling/text :text "t"})]))

(def ^:private gen-step
  (gen/tuple (gen/elements ["a" "b" "c"]) (gen/choose 0 250) gen-event))

(defn- run-steps [steps window-ms]
  (let [hx (harness {:window-ms window-ms})]
    (reduce (fn [t [id dt e]]
              (let [t (+ t dt)] (advance! hx t) (feed! hx id e) t))
            0 steps)
    (advance! hx (+ 1 window-ms (reduce + (map second steps))))
    @(:emitted hx)))

(defn- by-ling [emitted]
  (group-by second emitted))

(defspec at-most-one-update-per-ling-per-window 200
  (prop/for-all [steps (gen/vector gen-step 0 40)]
    (let [window 100]
      (every? (fn [[_ sends]]
                (every? (fn [[[t1] [t2]]] (>= (- t2 t1) window))
                        (partition 2 1 sends)))
              (by-ling (run-steps steps window))))))

(defspec the-last-value-always-wins 200
  (prop/for-all [steps (gen/vector gen-step 0 40)]
    (let [sent (by-ling (run-steps steps 100))]
      (every? (fn [id]
                (let [expected (pg/slave-update
                                (d/summarize (keep (fn [[i _ e]] (when (= i id) e)) steps)))
                      last-sent (last (map #(nth % 2) (get sent id)))]
                  (if (empty? expected) (nil? last-sent) (= expected last-sent))))
              ["a" "b" "c"]))))

(defspec consecutive-updates-differ 200
  (prop/for-all [steps (gen/vector gen-step 0 40)]
    (every? (fn [[_ sends]]
              (every? (fn [[[_ _ u1] [_ _ u2]]] (not= u1 u2)) (partition 2 1 sends)))
            (by-ling (run-steps steps 100)))))

;; =============================================================================
;; Real scheduler and addon wiring
;; =============================================================================

(defn- await-until [pred ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 5) (recur))))))

(deftest owned-scheduler-flushes-the-trailing-value
  (let [emitted (atom [])
        sink (pg/progress-sink {:window-ms 30 :emit! (fn [id u] (swap! emitted conj [id u]))})]
    (try
      (doseq [k (range 6)] (pg/on-event! sink "l1" (plan k 5)))
      (is (await-until #(= ["l1" (progress-of 5 5)] (last @emitted)) 2000))
      (is (= [["l1" (progress-of 0 5)] ["l1" (progress-of 5 5)]] @emitted))
      (finally (pg/close! sink)))))

(def ^:private sid "sess-1")

(defn- scripted-session [opts]
  (acp/acp-session
   (fa/scripted-agent
    {"initialize" (fn [ag f] (fa/reply! ag f {"protocolVersion" 1}))
     "session/new" (fn [ag f] (fa/reply! ag f {"sessionId" sid}))
     "session/prompt" (fn [ag f]
                        (fa/update! ag sid {"sessionUpdate" "plan"
                                            "entries" [{"content" "a" "status" "completed"}
                                                       {"content" "b" "status" "pending"}]})
                        (fa/reply! ag f {"stopReason" "end_turn"
                                         "_meta" {"usage" {"costUsd" 0.25}}}))})
   opts))

(deftest addon-feeds-session-events-to-the-progress-writer
  (let [writes (atom [])
        seen (atom [])
        a (ling-addon/addon-ctor
           {:ling/register! (fn [id _ _] {:registered? true :headless-id id})
            :ling/deregister! (fn [_])
            :ling/make-session scripted-session
            :ling/on-event (fn [id e] (swap! seen conj [id (:event e)]))
            :ling/progress! (fn [id u] (swap! writes conj [id u]))
            :ling/progress-window-ms 20})
        init (addon/initialize! a {})
        be ((:ling/backend (addon/hooks a)))]
    (try
      (is (true? (get-in init [:metadata :progress?])))
      (h/headless-spawn! be {:id "l1"} {:task "go"})
      (is (await-until #(= ["l1" (assoc (progress-of 1 2) :ling/cost-usd 0.25)] (last @writes)) 2000))
      (is (some #{["l1" :ling/turn-end]} @seen) "the caller's on-event still runs")
      (finally (addon/shutdown! a)))))

(deftest addon-progress-can-be-turned-off
  (let [a (ling-addon/addon-ctor {:ling/register! (fn [id _ _] {:registered? true :headless-id id})
                                  :ling/deregister! (fn [_])
                                  :ling/progress! nil})]
    (is (false? (get-in (addon/initialize! a {}) [:metadata :progress?])))
    (addon/shutdown! a)))
