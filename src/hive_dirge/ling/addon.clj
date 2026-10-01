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
     :ling/priority          registry priority (default 5)"
  (:require [hive-addon.protocol :as addon]
            [hive-dirge.ling.backend :as backend]))

;; SPDX-License-Identifier: MIT

(def addon-id "hive.dirge.ling")

(def default-priority 5)

(def host-register 'hive-mcp.agent.ling.headless-registry/register-headless!)

(def host-deregister 'hive-mcp.agent.ling.headless-registry/deregister-headless!)

(defn registry-meta
  "Registry metadata for CONFIG."
  [config]
  {:provides #{backend/backend-id}
   :priority (or (:ling/priority config) default-priority)})

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

(defn- start! [state seed runtime-config]
  (locking state
    (if (= :active (:lifecycle @state))
      {:success? true :already-initialized? true}
      (let [config (merge seed runtime-config)
            b (backend/dirge-backend {:make-session (:ling/make-session config)
                                      :defaults (:ling/session-defaults config)
                                      :on-event (:ling/on-event config)})
            outcome (register-backend! config b)
            registered? (boolean (:registered? outcome))]
        (reset! state {:lifecycle :active :backend b :config config
                       :registered? registered? :registration outcome})
        {:success? true
         :errors []
         :metadata {:headless-id backend/backend-id :registered? registered?}}))))

(defn- stop! [state]
  (locking state
    (let [{:keys [lifecycle backend config registered?]} @state]
      (when (= :active lifecycle)
        (backend/close-all! backend)
        (when registered? (deregister-backend! config)))
      (reset! state {:lifecycle :stopped})
      nil)))

(defn- health-of [state]
  (let [{:keys [lifecycle backend registered? registration]} @state]
    {:status (cond
               (not= :active lifecycle) :degraded
               registered? :ok
               :else :degraded)
     :details (cond-> {:lifecycle lifecycle}
                backend (assoc :sessions (count @(:sessions backend))
                               :registered? registered?)
                (and registration (not registered?)) (assoc :errors (:errors registration)))}))

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
