(ns hive-dirge.ling.progress
  "Progress sink: folds each ling's events into {:ling/progress {:done :total}
   :ling/cost-usd n} and hands that update to an emit! fn (fn [ling-id update])
   at most once per window per ling. A change inside the window is held and
   flushed when the window closes; the last value always wins.

   Pure core: observe, flush. Effects: progress-sink, on-event!, close!."
  (:require [hive-dirge.ling.domain :as d])
  (:import (java.util.concurrent ConcurrentHashMap Executors ScheduledExecutorService
                                 ThreadFactory TimeUnit)
           (java.util.function Function)))

;; SPDX-License-Identifier: MIT

(def default-window-ms 500)

;; =============================================================================
;; Pure
;; =============================================================================

(defn slave-update
  "Slave keys SUMMARY reports: plan progress when there is a plan, cost when known."
  [summary]
  (let [{:keys [total] :as progress} (:progress summary)
        cost (d/cost-usd summary)]
    (cond-> {}
      (pos? (or total 0)) (assoc :ling/progress (select-keys progress [:done :total]))
      (number? cost) (assoc :ling/cost-usd cost))))

(def empty-ling
  {:summary d/empty-summary :sent nil :sent-at nil :pending nil})

(defn- due? [{:keys [sent-at]} now window-ms]
  (or (nil? sent-at) (>= (- now sent-at) window-ms)))

(defn- send-now [ling update now]
  {:ling (assoc ling :sent update :sent-at now :pending nil) :emit update})

(defn observe
  "LING after EVENT at NOW: {:ling l :emit update-or-nil :flush-at ms-or-nil}.
   :flush-at is set when a held value first needs a trailing flush."
  [ling event now window-ms]
  (let [ling (cond-> (update ling :summary d/step event)
               (= :ling/closed (:event event)) (assoc :closed? true))
        u (slave-update (:summary ling))
        latest (or (:pending ling) (:sent ling))]
    (cond
      (or (empty? u) (= u latest)) {:ling ling}
      (due? ling now window-ms) (send-now ling u now)
      :else {:ling (assoc ling :pending u)
             :flush-at (when-not (:pending ling) (+ (:sent-at ling) window-ms))})))

(defn flush-ling
  "LING when its window closes at NOW: sends the held value unless it equals
   the one already sent. Early calls answer a new :flush-at."
  [ling now window-ms]
  (let [{:keys [pending sent]} ling]
    (cond
      (nil? pending) {:ling ling}
      (not (due? ling now window-ms)) {:ling ling :flush-at (+ (:sent-at ling) window-ms)}
      (= pending sent) {:ling (assoc ling :pending nil)}
      :else (send-now ling pending now))))

(defn drain-ling
  "LING at shutdown: sends the held value now, window or not."
  [ling now]
  (let [{:keys [pending sent]} ling]
    (if (and pending (not= pending sent))
      (send-now ling pending now)
      {:ling (assoc ling :pending nil)})))

;; =============================================================================
;; Effects
;; =============================================================================

(defn- daemon-scheduler ^ScheduledExecutorService []
  (Executors/newSingleThreadScheduledExecutor
   (reify ThreadFactory
     (newThread [_ r]
       (doto (Thread. ^Runnable r "hive-dirge-progress-sink")
         (.setDaemon true))))))

(defn- executor-schedule [^ScheduledExecutorService ex]
  (fn [delay-ms f]
    (when-not (.isShutdown ex)
      (.schedule ex ^Runnable f (long (max 0 delay-ms)) TimeUnit/MILLISECONDS))))

(defn- monotonic-ms []
  (quot (System/nanoTime) 1000000))

(defrecord ProgressSink [lings locks emit! clock schedule! window-ms executor errors])

(defn progress-sink
  "A ProgressSink. Opts: :emit! (fn [ling-id update]) required; :window-ms
   (default 500); :clock (fn [] ms); :schedule! (fn [delay-ms f]), default a
   daemon scheduler owned by the sink."
  [{:keys [emit! window-ms clock schedule!]}]
  (let [executor (when-not schedule! (daemon-scheduler))]
    (->ProgressSink (atom {}) (ConcurrentHashMap.) emit!
                    (or clock monotonic-ms)
                    (or schedule! (executor-schedule executor))
                    (or window-ms default-window-ms)
                    executor
                    (atom []))))

(def ^:private new-lock
  (reify Function (apply [_ _] (Object.))))

(defn- lock-of [{:keys [^ConcurrentHashMap locks]} id]
  (.computeIfAbsent locks id new-lock))

(defn- emit-safely! [{:keys [emit! errors]} id update]
  (try (emit! id update)
       (catch Throwable t
         (swap! errors conj {:ling-id id :update update :error (ex-message t)}))))

(declare flush!)

(defn- apply-step!
  "Runs STEP on ling ID under its lock, then emits and schedules as told. A
   closed ling with nothing held is forgotten."
  [sink id step]
  (let [lock (lock-of sink id)]
    (locking lock
      (let [{:keys [lings schedule! clock]} sink
            ling (get @lings id empty-ling)
            {next-ling :ling emit :emit flush-at :flush-at} (step ling)]
        (if (and (:closed? next-ling) (nil? (:pending next-ling)))
          (swap! lings dissoc id)
          (swap! lings assoc id next-ling))
        (when emit (emit-safely! sink id emit))
        (when flush-at
          (schedule! (- flush-at (clock)) (fn [] (flush! sink id))))))))

(defn flush!
  "Closes the window of ling ID: sends its held value if any."
  [sink id]
  (apply-step! sink id #(flush-ling % ((:clock sink)) (:window-ms sink))))

(defn on-event!
  "Feeds EVENT of ling ID to SINK."
  [sink id event]
  (apply-step! sink id #(observe % event ((:clock sink)) (:window-ms sink))))

(defn on-event-fn
  "SINK as a (fn [ling-id event]) for the backend's :on-event."
  [sink]
  (fn [id event] (on-event! sink id event)))

(defn close!
  "Sends every held value now, forgets every ling, stops the owned scheduler."
  [sink]
  (doseq [id (keys @(:lings sink))]
    (apply-step! sink id #(drain-ling % ((:clock sink)))))
  (reset! (:lings sink) {})
  (when-let [^ScheduledExecutorService ex (:executor sink)]
    (.shutdownNow ex))
  nil)

(defn errors
  "Emit failures SINK has recorded."
  [sink]
  @(:errors sink))
