(ns hive-dirge.sense.relay
  "Effects of the sense relay: sixth-sense senses pushed into dirge's agent
   loop as loop ops over the vessel SSE bridge.

   Flow: sixth-sense stores a sense and calls the relay's listener; the
   listener only schedules a PUMP on the relay's own thread. A pump, when a
   connected dirge client advertises the `loop` feature, drains the
   consume-once batch for consumer \"dirge\", turns each sense into its loop
   op (hive-dirge.sense.domain), holds it as pending and broadcasts it.
   dirge acks an op once the op is in its loop; a (re)connecting client
   first gets every pending op again, then a fresh pump. With no loop-capable
   client nothing is drained, so senses wait in sixth-sense (persisted) and
   the coordinator's other readers are unaffected.

   Every effect goes through the map given to `start!`:
     :source      an ISenseSource (nil = no sixth-sense here: relay idle)
     :broadcast!  (fn [op]) sends one op to every connected client
     :loop-client? (fn [] bool) is any connected client loop-capable
     :policy      class -> mode (hive-dirge.sense.domain/->policy)
     :receptor    passed to drain! (nil = sixth-sense's tuning)"
  (:require [hive-dirge.sense.domain :as domain]
            [hive-dirge.sense.ports :as ports])
  (:import (java.util.concurrent ExecutorService Executors ThreadFactory TimeUnit)))

;; SPDX-License-Identifier: MIT

(def listener-key ::relay)

;; =============================================================================
;; sixth-sense adapter
;; =============================================================================

(defn- resolve-api [sym]
  (try (requiring-resolve (symbol "hive-agent.sixth-sense.api" (name sym)))
       (catch Throwable _ nil)))

(defrecord SixthSenseSource [listen-fn unlisten-fn port-fn]
  ports/ISenseSource
  (listen! [_ key f] (listen-fn key f))
  (unlisten! [_ key] (unlisten-fn key))
  (drain! [_ consumer receptor]
    (:senses ((:drain! (port-fn)) {:consumer consumer :receptor receptor}))))

(defn sixth-sense-source
  "The process sixth-sense as an ISenseSource, or nil when hive-agent (or a
   sixth-sense without the listen! seam) is not on the classpath."
  []
  (let [listen (resolve-api 'listen!)
        unlisten (resolve-api 'unlisten!)
        port (resolve-api 'sense-port)]
    (when (and listen unlisten port)
      (->SixthSenseSource listen unlisten port))))

;; =============================================================================
;; Relay
;; =============================================================================

(defn- relay-thread []
  (reify ThreadFactory
    (newThread [_ r]
      (doto (Thread. ^Runnable r "hive-dirge-sense")
        (.setDaemon true)))))

(defn- submit! [{:keys [^ExecutorService pool]} f]
  (try (.execute pool ^Runnable (fn [] (try (f) (catch Throwable _ nil)))) true
       (catch Throwable _ false)))

(defn- send! [{:keys [broadcast! state]} op]
  (let [r (try (broadcast! op) (catch Throwable _ nil))]
    (swap! state update :sent (fnil inc 0))
    r))

(defn pump!
  "Drain and send every new sense now, on the calling thread. Returns the
   number of ops sent. Nothing is drained without a loop-capable client."
  [{:keys [source loop-client? policy receptor state] :as relay}]
  (if (and source (loop-client?))
    (let [senses (ports/drain! source domain/consumer receptor)
          ops (keep #(domain/sense->op policy %) senses)]
      (swap! state update :ignored (fnil + 0) (- (count senses) (count ops)))
      (doseq [op ops]
        (swap! state update :pending domain/hold op)
        (send! relay op))
      (count ops))
    0))

(defn replay!
  "Send every pending op again, oldest first. Returns how many."
  [{:keys [state] :as relay}]
  (let [ops (domain/unacked (:pending @state))]
    (doseq [op ops] (send! relay op))
    (count ops)))

(defn wake!
  "Schedule a pump on the relay thread (what the sixth-sense listener calls)."
  [relay]
  (submit! relay #(pump! relay)))

(defn connected!
  "A client (re)connected: replay pending ops, then pump."
  [relay]
  (submit! relay #(do (replay! relay) (pump! relay))))

(defn ack!
  "dirge injected the op for SENSE-ID; stop holding it. Returns true when it
   was pending."
  [{:keys [state]} sense-id]
  (let [[before] (swap-vals! state update :pending domain/ack sense-id)]
    (when (contains? (get-in before [:pending :ops]) sense-id)
      (swap! state update :acked (fnil inc 0))
      true)))

(defn- load-held
  "The pending set PENDING-STORE saved, normalized; empty without a store or
   when the store cannot be read."
  [pending-store]
  (if pending-store
    (domain/restore (try (ports/load-pending pending-store)
                         (catch Throwable _unreadable-store-starts-empty nil)))
    domain/empty-pending))

(defn- persist-pending!
  "Save the held set to the relay's store whenever it changes. The save reads
   the CURRENT set under a lock, so two threads changing it (a pump and an ack)
   cannot leave the older set as the last one written."
  [{:keys [pending-store state]}]
  (when pending-store
    (add-watch state ::persist
               (fn [_ _ old new]
                 (when (not= (:pending old) (:pending new))
                   (locking pending-store
                     (try (ports/save-pending! pending-store (:pending @state))
                          (catch Throwable _counted-in-status
                            (swap! state update :save-failed (fnil inc 0))))))))))

(defn start!
  "A running relay (see ns doc for OPTS). Registers the sixth-sense
   listener and schedules a first pump for a backlog. With a :pending-store
   the held set is seeded from it (normalized) and saved on every change."
  [{:keys [source policy pending-store] :as opts}]
  (let [relay (assoc opts
                     :policy (or policy domain/default-policy)
                     :state (atom {:pending (load-held pending-store)
                                   :sent 0 :acked 0 :ignored 0 :save-failed 0})
                     :pool (Executors/newSingleThreadExecutor (relay-thread)))]
    (persist-pending! relay)
    (when source
      (ports/listen! source listener-key (fn [_sense] (wake! relay)))
      (wake! relay))
    relay))

(defn stop!
  [{:keys [source ^ExecutorService pool]}]
  (when source (try (ports/unlisten! source listener-key) (catch Throwable _ nil)))
  (when pool
    (.shutdown pool)
    (.awaitTermination pool 2 TimeUnit/SECONDS))
  nil)

(defn status
  "What health may show about RELAY. A relay with a pending store also
   reports :durable? true and how many saves failed."
  [{:keys [source state pending-store]}]
  (let [{:keys [pending sent acked ignored save-failed]} @state]
    (cond-> {:senses (if source :listening :absent)
             :pending (count (:order pending))
             :sent sent :acked acked :ignored ignored}
      pending-store (assoc :durable? true :save-failed (or save-failed 0)))))
