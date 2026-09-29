(ns hive-dirge.economy.addon
  "hive.dirge.economy: logs every tool result under a content Handle, serves
   it back through the context_retrieve tool, and answers dirge's compact hook
   with a structured, citing Digest. Wiring only."
  (:require [hive-addon.protocol :as p]
            [hive-dirge.economy.adapters.local :as local]
            [hive-dirge.economy.pipeline.compact :as compact]
            [hive-dirge.economy.pipeline.observe :as observe]
            [hive-dirge.economy.pipeline.retrieve :as retrieve]
            [hive-dirge.economy.registry :as registry]
            [hive-dirge.harness :as h]))

(def addon-id-str "hive.dirge.economy")

(def harness-ports
  {:cwd h/cwd})

(defn- session-path
  [ports state]
  (let [{:keys [session-id cwd]} @state]
    (local/spill-path (or cwd ((:cwd ports))) session-id)))

(defn- fresh-session-id
  []
  (str "s" (rand-int 2147483647)))

(defn make-env
  "{:log :stats :digests} for the pipelines; the log spills under the session
   cwd. :digests holds each session's last Digest (carry-forward)."
  [ports state]
  {:log     (local/make-log (local/file-spill #(session-path ports state)))
   :stats   (atom {})
   :digests (atom {})})

(defn compact-env
  "`env` plus the Digestor the addon config selects (registry, OCP) and the
   optional :now clock port."
  [env ports state]
  (let [config (:config @state)]
    (assoc env
           :config config
           :digestor (registry/select :digestor config)
           :now (:now ports))))

(def retrieve-tool-schema
  {:type       "object"
   :properties {:handle {:type "string" :description "handle, e.g. §1a2b3c4d"}
                :start  {:type "integer" :description "first line (or char), 1-based"}
                :end    {:type "integer" :description "last line (or char), inclusive"}
                :unit   {:type "string" :enum ["lines" "chars"]}}
   :required   ["handle"]})

(defn tool-defs
  [env]
  [{:name        "context_retrieve"
    :description "Fetch the full text of an earlier tool result by its handle, optionally a line or char range."
    :inputSchema retrieve-tool-schema
    :handler     (fn [params] (retrieve/tool-answer (retrieve/retrieve env params)))}])

(defn- session-start
  [state ctx]
  (swap! state assoc
         :session-id (or (:session-id ctx) (:session-id @state))
         :cwd (or (:cwd ctx) (:cwd @state)))
  nil)

(defrecord HiveDirgeEconomyAddon [state ports env]
  p/IAddon
  (addon-id [_] addon-id-str)
  (addon-type [_] :native)
  (capabilities [_] #{:tools :health-reporting :dirge/hooks :context/economy})
  (initialize! [_ config]
    (swap! state assoc
           :config (or (:addon/config config) (:config @state))
           :session-id (or (:session-id @state) (fresh-session-id))
           :initialized? true)
    {:success? true :errors [] :metadata {:addon/id addon-id-str}})
  (shutdown! [_]
    (swap! state assoc :initialized? false)
    {:success? true :errors []})
  (tools [_] (tool-defs env))
  (schema-extensions [_] {})
  (health [_]
    (if (:initialized? @state)
      {:status :ok :details (merge @(:stats env) (local/log-stats (:log env)))}
      {:status :down :details {:reason :not-initialized}}))
  (excluded-tools [_] #{})
  (hooks [_]
    {:dirge/session-start   (fn [ctx] (session-start state ctx))
     :dirge/after-tool-call (fn [ctx] (observe/after-tool-call env ctx))
     :dirge/before-compact  (fn [ctx] (compact/before-compact env ctx))
     :dirge/compact         (fn [ctx] (compact/compact (compact-env env ports state) ctx))}))

(defn make-addon
  ([ports] (make-addon ports {}))
  ([ports config]
   (let [state (atom {:initialized? false :config config})]
     (->HiveDirgeEconomyAddon state ports (make-env ports state)))))

(defn addon-ctor
  "Manifest :addon/init-fn."
  [config]
  (make-addon harness-ports (if (map? config) config {})))
