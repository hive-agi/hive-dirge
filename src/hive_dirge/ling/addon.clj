(ns hive-dirge.ling.addon
  "hive.dirge.ling IAddon: builds a DirgeHeadlessBackend on initialize! and
   registers it as :dirge with the host's headless registry, resolved at
   runtime; shutdown! closes every session and deregisters.

   Config (manifest :addon/config merged with runtime config):
     :ling/register!         (fn [id backend meta] -> {:registered? bool}),
                             default the host's register-headless!
     :ling/deregister!       (fn [id]), default the host's deregister-headless!
     :ling/make-session      session factory, see backend/dirge-backend
     :ling/session-defaults  session opts under every spawn
     :ling/on-event          (fn [ling-id event])
     :ling/progress!         (fn [ling-id slave-update]), default the host's
                             update-slave!; nil turns the progress sink off
     :ling/progress-window-ms  progress coalescing window (default 500)
     :ling/priority          registry priority (default 5)"
  (:require [hive-addon.protocol :as addon]
            [hive-dirge.ling.backend :as backend]
            [hive-dirge.ling.progress :as progress]))

;; SPDX-License-Identifier: MIT

(def addon-id "hive.dirge.ling")

(def default-priority 5)

(def host-register 'hive-mcp.agent.ling.headless-registry/register-headless!)

(def host-deregister 'hive-mcp.agent.ling.headless-registry/deregister-headless!)

(def host-update-slave 'hive-mcp.swarm.registry/update-slave!)

(def host-register-mode 'hive-spi.swarm.spawn-modes/register-mode!)

(def host-deregister-mode 'hive-spi.swarm.spawn-modes/deregister-mode!)

(defn registry-meta
  "Registry metadata for CONFIG."
  [config]
  {:provides #{backend/backend-id}
   :priority (or (:ling/priority config) default-priority)})

(defn spawn-mode-spec
  "Spawn-mode registry entry for :dirge under CONFIG: a headless subprocess
   mode, visible on the MCP spawn_mode enum, carrying the backend's
   capabilities. PURE."
  [config]
  {:description     "dirge over ACP on stdio (hive.dirge.ling addon)"
   :requires-emacs? false
   :io-model        :stdin-stdout
   :slot-limit      (:ling/slot-limit config)
   :mcp?            true
   :alias-of        nil
   :capabilities    (into #{:dispatch :kill :cost-tracking} backend/capabilities)})

(defn- soft-resolve
  "The var named by SYM, or nil when its namespace is not on the classpath."
  [sym]
  (try (requiring-resolve sym)
       (catch Throwable host-absent
         (when-not (instance? java.io.FileNotFoundException host-absent)
           (binding [*out* *err*]
             (println "hive.dirge.ling: could not load" sym "-" (ex-message host-absent))))
         nil)))

(defn- register-backend! [config b]
  (if-let [register! (or (:ling/register! config) (soft-resolve host-register))]
    (try (or (register! backend/backend-id b (registry-meta config))
             {:registered? false :errors ["registry answered nil"]})
         (catch Throwable t
           {:registered? false :errors [(ex-message t)]}))
    {:registered? false :errors ["no headless registry on the classpath"]}))

(defn- deregister-backend! [config]
  (when-let [deregister! (or (:ling/deregister! config) (soft-resolve host-deregister))]
    (try (deregister! backend/backend-id)
         (catch Throwable t
           (ex-message t)))))

(defn- register-mode!
  "Registers :dirge as a spawn mode; true when a registry took it."
  [config]
  (when-let [register! (or (:ling/register-mode! config) (soft-resolve host-register-mode))]
    (try (register! backend/backend-id (spawn-mode-spec config))
         true
         (catch Throwable t
           (binding [*out* *err*]
             (println "hive.dirge.ling: could not register the spawn mode -" (ex-message t)))
           false))))

(defn- deregister-mode! [config]
  (when-let [deregister! (or (:ling/deregister-mode! config) (soft-resolve host-deregister-mode))]
    (try (deregister! backend/backend-id)
         (catch Throwable t
           (ex-message t)))))

(defn- progress-sink
  "The progress sink for CONFIG, or nil when progress is off or no writer resolves."
  [config]
  (let [emit! (:ling/progress! config)
        emit! (if (contains? config :ling/progress!) emit! (soft-resolve host-update-slave))]
    (when emit!
      (progress/progress-sink {:emit! emit! :window-ms (:ling/progress-window-ms config)}))))

(defn- compose-on-event [sink on-event]
  (let [feed (some-> sink progress/on-event-fn)]
    (cond
      (and feed on-event) (fn [id e] (feed id e) (on-event id e))
      :else (or feed on-event))))

(defn- start! [state seed runtime-config]
  (locking state
    (if (= :active (:lifecycle @state))
      {:success? true :already-initialized? true}
      (let [config (merge seed runtime-config)
            sink (progress-sink config)
            b (backend/dirge-backend {:make-session (:ling/make-session config)
                                      :defaults (:ling/session-defaults config)
                                      :on-event (compose-on-event sink (:ling/on-event config))})
            outcome (register-backend! config b)
            registered? (boolean (:registered? outcome))
            mode? (boolean (when registered? (register-mode! config)))]
        (reset! state {:lifecycle :active :backend b :progress sink :config config
                       :registered? registered? :registration outcome
                       :spawn-mode? mode?})
        {:success? true
         :errors []
         :metadata {:headless-id backend/backend-id :registered? registered?
                    :spawn-mode? mode? :progress? (some? sink)}}))))

(defn- stop! [state]
  (locking state
    (let [{:keys [lifecycle backend progress config registered? spawn-mode?]} @state]
      (when (= :active lifecycle)
        (backend/close-all! backend)
        (some-> progress progress/close!)
        (when spawn-mode? (deregister-mode! config))
        (when registered? (deregister-backend! config)))
      (reset! state {:lifecycle :stopped})
      nil)))

(defn- health-of [state]
  (let [{:keys [lifecycle backend progress registered? registration]} @state]
    {:status (cond
               (not= :active lifecycle) :degraded
               registered? :ok
               :else :degraded)
     :details (cond-> {:lifecycle lifecycle}
                backend (assoc :sessions (count @(:sessions backend))
                               :registered? registered?)
                (and registration (not registered?)) (assoc :errors (:errors registration))
                (seq (some-> progress progress/errors)) (assoc :progress-errors (count (progress/errors progress))))}))

(defrecord DirgeLingAddon [state seed]
  addon/IAddon
  (addon-id [_] addon-id)
  (addon-type [_] :native)
  (capabilities [_] #{:headless-backend :health-reporting})
  (initialize! [_ runtime-config] (start! state seed runtime-config))
  (shutdown! [_] (stop! state))
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] (health-of state))
  (excluded-tools [_] #{})
  (hooks [_]
    (if-let [b (when (= :active (:lifecycle @state)) (:backend @state))]
      {:ling/backend (fn [] b)}
      {})))

(defn addon-ctor
  "Pure constructor resolved from the mount manifest: no process or thread."
  [config]
  (->DirgeLingAddon (atom {:lifecycle :created}) (or config {})))
