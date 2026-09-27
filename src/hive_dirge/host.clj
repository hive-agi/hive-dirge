(ns hive-dirge.host
  "hive.dirge.host IAddon: the JVM side of the dirge integration.

   initialize! starts hive-vessel's SSE bridge on loopback (random port,
   random token, every browser Origin refused), writes the discovery file
   $XDG_RUNTIME_DIR/hive-vessel/dirge.json {url, token, ...} as 0600, and
   exposes the :dirge vessel target through the hooks hive-olympus.harness
   folds: :vessel/dispatch! and :vessel/target. dirge reads the discovery
   file, subscribes to <url>/events?token=.. and POSTs key presses to
   <url>/reply?token=..; each reply is validated, answered at once (202, or
   4xx for a bad token or body, 503 when the queue is full) and queued; one
   worker routes the queued commands to hive.olympus in arrival order
   (:olympus/focus! :olympus/next-tab! :olympus/prev-tab! :olympus/refresh!),
   and the re-render reaches dirge on the SSE stream.

   Config (manifest :addon/config merged with runtime config):
     :dirge/port             bridge port (default 0 = random)
     :dirge/discovery-path   discovery file (default under $XDG_RUNTIME_DIR)
     :dirge/heartbeat-ms     SSE ': ping' period (default hive-vessel's)
     :dirge/olympus          an IOlympusControl to route replies to (default:
                             hive.olympus's hooks, from :mount/dependencies)
     :dirge/action-queue     (fn [run-command] IActionQueue) holding accepted
                             commands (default: one worker thread over a
                             FIFO of domain/reply-queue-capacity)

   The token is only ever written to the 0600 discovery file; health and
   metadata never carry it."
  (:require [hive-addon.protocol :as addon]
            [hive-dirge.host.boundary :as boundary]
            [hive-dirge.host.domain :as domain]
            [hive-dirge.host.ports :as ports]
            [hive-vessel.core :as v]))

;; SPDX-License-Identifier: MIT

(def reply-log-size 20)

(defn- dependency-hooks [config]
  (keep (fn [[_ dep]]
          (when (addon/addon? dep)
            (try (addon/hooks dep) (catch Throwable _ nil))))
        (:mount/dependencies config)))

(defn- olympus-port [config]
  (or (:dirge/olympus config)
      (boundary/hooks-olympus (fn [] (boundary/olympus-hooks config)))))

(defn- record! [state outcome]
  (swap! state update :replies
         (fn [rs] (vec (take-last reply-log-size (conj (or rs []) (dissoc outcome :result))))))
  outcome)

(defn- run-command-fn
  "What the worker applies to each queued command: route it through OLYMPUS
   and record the outcome."
  [state olympus]
  (fn [command] (record! state (ports/route! olympus command))))

(defn- action-queue [config run-command]
  (if-let [make (:dirge/action-queue config)]
    (make run-command)
    (boundary/single-worker-queue {:capacity domain/reply-queue-capacity
                                   :run-command run-command})))

(defn- on-reply-fn
  "Raw POST body -> parsed command -> offered to QUEUE; returns the HTTP
   status at once. Refusals are recorded here, routed outcomes by the worker."
  [state queue]
  (fn [raw]
    (let [command (domain/parse-reply raw)
          outcome (domain/reply-outcome command
                                        (and (not (:reply/error command))
                                             (ports/submit! queue command)))]
      (when (:reply/error outcome) (record! state outcome))
      (domain/reply-status outcome))))

(defn- start! [state seed runtime-config]
  (locking state
    (if (= :active (:lifecycle @state))
      {:success? true :already-initialized? true}
      (let [config (merge seed runtime-config)
            token (boundary/new-token)
            path (or (:dirge/discovery-path config)
                     (domain/discovery-path (System/getenv "XDG_RUNTIME_DIR")))
            queue (action-queue config (run-command-fn state (olympus-port config)))
            bridge (boundary/start-bridge! {:port (:dirge/port config)
                                            :token token
                                            :heartbeat-ms (:dirge/heartbeat-ms config)
                                            :on-reply (on-reply-fn state queue)})]
        (try
          (boundary/write-private! path (domain/discovery-json
                                         {:port (:port bridge) :token token
                                          :pid (.pid (java.lang.ProcessHandle/current))}))
          (let [registry (atom (v/registry-from-hooks (dependency-hooks config)))
                target (domain/target (boundary/executor bridge))]
            (swap! state assoc
                   :lifecycle :active :bridge bridge :queue queue :discovery path
                   :registry registry :target target :replies [])
            {:success? true
             :errors []
             :metadata {:port (:port bridge) :discovery path}})
          (catch Throwable t
            (boundary/stop-bridge! bridge)
            (ports/close! queue)
            (reset! state {:lifecycle :failed :last-error (ex-message t)})
            {:success? false :errors [(ex-message t)]}))))))

(defn- stop! [state]
  (locking state
    (let [{:keys [lifecycle bridge queue discovery]} @state]
      (when (= :active lifecycle)
        (boundary/stop-bridge! bridge)
        (ports/close! queue)
        (try (boundary/delete-file! discovery) (catch Throwable _ nil)))
      (reset! state {:lifecycle :stopped})
      nil)))

(defn- health-of [state]
  (let [{:keys [lifecycle bridge discovery replies last-error]} @state]
    {:status (case lifecycle
               :active (if (pos? (:clients (boundary/bridge-status bridge))) :ok :degraded)
               :failed :down
               :degraded)
     :details (cond-> {:lifecycle lifecycle}
                bridge (merge (boundary/bridge-status bridge)
                              {:discovery discovery
                               :replies (count replies)
                               :reply-errors (count (filter :reply/error replies))})
                last-error (assoc :last-error last-error))}))

(defrecord DirgeHostAddon [state seed]
  addon/IAddon
  (addon-id [_] domain/addon-id)
  (addon-type [_] :native)
  (capabilities [_] #{:vessel :health-reporting})
  (initialize! [_ runtime-config] (start! state seed runtime-config))
  (shutdown! [_] (stop! state))
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] (health-of state))
  (excluded-tools [_] #{})
  (hooks [_]
    (let [{:keys [lifecycle bridge registry target]} @state]
      (if (= :active lifecycle)
        {:vessel/target (fn [] target)
         :vessel/dispatch! (fn [op-or-ops] (v/dispatch! registry target op-or-ops))
         :vessel/register-translators! (fn [translators] (swap! registry v/register-all translators) nil)
         :dirge/bridge (fn [] (boundary/bridge-status bridge))
         :dirge/replies (fn [] (:replies @state))}
        {}))))

(defn addon-ctor
  "Pure constructor resolved from the mount manifest: no port, file or thread."
  [config]
  (->DirgeHostAddon (atom {:lifecycle :created}) (or config {})))
