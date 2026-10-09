(ns hive-dirge.economy.addon
  "hive.dirge.economy: logs every tool result under a content Handle, serves
   it back through the context_retrieve tool, and answers dirge's compact hook
   with a structured, citing Digest. Wiring only."
  (:require [hive-addon.protocol :as p]
            [hive-dirge.economy.adapters.hive-memory :as hive-memory]
            [hive-dirge.economy.adapters.local :as local]
            [hive-dirge.economy.pipeline.compact :as compact]
            [hive-dirge.economy.pipeline.crystallize :as crystallize]
            [hive-dirge.economy.pipeline.retrieve :as retrieve]
            [hive-dirge.economy.registry :as registry]
            [hive-dirge.harness :as h]
            [hive-dirge.economy.pipeline.watch :as watch]
            [hive-dirge.live :as live]))

(def addon-id-str "hive.dirge.economy")

(def harness-ports
  {:cwd        h/cwd
   :mcp-call   h/mcp-call
   :json-parse h/json-parse})

(defn- session-path
  [ports state]
  (let [{:keys [session-id cwd]} @state]
    (local/spill-path (or cwd ((:cwd (live/ports ports)))) session-id)))

(defn- fresh-session-id
  []
  (str "s" (rand-int 2147483647)))

(defn make-env
  "{:log :stats :digests :pending} for the pipelines; the log spills under the
   session cwd. :digests holds each session's last Digest (carry-forward),
   :pending the args of tool calls whose result has not arrived yet."
  [ports state]
  {:log     (local/make-log (local/file-spill #(session-path ports state)))
   :stats   (atom {})
   :digests (atom {})
   :pending (atom {})})

(defn crystallizer
  "The ICrystallizer over hive memory, built from the live ports; nil
   outside dirge (no :mcp-call port)."
  [ports state]
  (let [p (live/ports ports)]
    (hive-memory/make-crystallizer {:server     (:hive/server (:config @state))
                                    :mcp-call   (:mcp-call p)
                                    :json-parse (:json-parse p)})))

(defn compact-env
  "`env` plus the Digestor the addon config selects (registry, OCP), the
   optional :now clock port and the crystallizer."
  [env ports state]
  (let [config (:config @state)]
    (assoc env
           :config config
           :digestor (registry/select :digestor config)
           :crystallizer (crystallizer ports state)
           :now (:now (live/ports ports)))))

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
  "Records the session; when :economy/crystallize? is on, answers
   {:context text} seeded from the project's newest Digests."
  [env ports state ctx]
  (swap! state assoc
         :session-id (or (:session-id ctx) (:session-id @state))
         :cwd (or (:cwd ctx) (:cwd @state)))
  (crystallize/seed (compact-env env ports state) (:cwd @state)))

(defn tool-list
  "The addon's tools, built per call so a refresh sees REPL redefinitions."
  [{:keys [env]}]
  (live/tools-with addon-id-str (tool-defs env)))

(defn hook-map
  "The addon's hooks, built per call so a refresh sees REPL redefinitions.
   Tool results, turns, usage and compactions are watched on :dirge/event.
   A Digest the compact hook answers is crystallized when enabled."
  [{:keys [state ports env]}]
  (live/hooks-with
   addon-id-str
   {:dirge/session-start  (fn [ctx] (session-start env ports state ctx))
    :dirge/event          (fn [ctx] (watch/on-event env ctx))
    :dirge/before-compact (fn [ctx] (compact/before-compact env ctx))
    :dirge/compact        (fn [ctx]
                            (let [cenv (compact-env env ports state)
                                  ctx' (update ctx :cwd #(or % (:cwd @state)))]
                              (crystallize/after-compact! cenv ctx'
                                                          (compact/compact cenv ctx))))}))

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
  (tools [this] (tool-list this))
  (schema-extensions [_] {})
  (health [_]
    (if (:initialized? @state)
      {:status :ok :details (merge @(:stats env) (local/log-stats (:log env)))}
      {:status :down :details {:reason :not-initialized}}))
  (excluded-tools [_] #{})
  (hooks [this] (hook-map this)))

(defn make-addon
  ([ports] (make-addon ports {}))
  ([ports config]
   (let [state (atom {:initialized? false :config config})]
     (->HiveDirgeEconomyAddon state ports (make-env ports state)))))

(defn addon-ctor
  "Manifest :addon/init-fn. Ports are read from `harness-ports` per call and
   the instance is registered in hive-dirge.live for the REPL."
  [config]
  (live/register! addon-id-str
                  (make-addon (fn [] harness-ports) (if (map? config) config {}))))
