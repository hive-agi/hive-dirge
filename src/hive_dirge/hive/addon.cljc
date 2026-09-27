(ns hive-dirge.hive.addon
  "hive.dirge: the in-dirge IAddon that connects a dirge session to hive over
   dirge's own MCP connection. Contributes the /hive command family
   (catchup, wrap, kanban, swarm), two tools proxying hive memory search and the
   kanban list, a static system-prompt hook, and the session hooks that run
   catchup at session start and wrap at session end.

   Effects go through a ports map {:mcp-call :json-parse :panel! :cwd} so the
   pipeline is testable with plain fns; `harness-ports` binds them to
   dirge.harness, which makes every effect a no-op outside dirge."
  (:require [hive-addon.protocol :as p]
            [hive-dirge.harness :as h]
            [hive-dirge.hive.domain :as d]))

(def addon-id-str "hive.dirge")

(def harness-ports
  "Ports bound to the dirge.harness natives."
  {:mcp-call   h/mcp-call
   :json-parse h/json-parse
   :panel!     h/panel!
   :cwd        h/cwd})

(defn- call!
  [ports [server tool args]]
  ((:mcp-call ports) server tool args))

(defn- directory
  [ports ctx]
  (or (:cwd ctx) ((:cwd ports))))

;; ---------------------------------------------------------------------------
;; /hive command pipeline

(defn- kanban!
  [ports config ctx status]
  (let [dir    (directory ports ctx)
        answer (call! ports (d/kanban-request config dir status))]
    (if-let [err (d/result-error answer)]
      {:text (str "/hive kanban failed: " err)}
      (let [text    (d/result-text answer)
            rows    (d/kanban-rows ((:json-parse ports) text))
            project (d/project-hint dir)]
        ((:panel! ports) (d/kanban-panel rows text project status))
        {:text (d/kanban-summary rows project)}))))

(defn- swarm!
  [ports config ctx scope]
  (let [dir    (directory ports ctx)
        scope  (or scope (:hive/swarm-scope config))
        answer (call! ports (d/swarm-request config dir scope))]
    (if-let [err (d/result-error answer)]
      {:text (str "/hive swarm failed: " err)}
      (let [text (d/result-text answer)
            rows (d/swarm-rows ((:json-parse ports) text))]
        ((:panel! ports) (d/swarm-panel rows text scope))
        {:text (d/swarm-summary rows)}))))

(defn run-command
  "Handles one /hive invocation against `ports`. Answers a dirge command
   reply {:text ..} or {:text .. :prompt ..}."
  [ports config ctx]
  (let [{:keys [action status scope reason]} (d/parse-command ctx)]
    (case action
      :help    {:text d/usage}
      :unknown {:text (str reason "\n" d/usage)}
      :catchup (d/command-reply config :catchup
                                (call! ports (d/catchup-request config (directory ports ctx))))
      :wrap    (d/command-reply config :wrap
                                (call! ports (d/wrap-request config (directory ports ctx))))
      :kanban  (kanban! ports config ctx status)
      :swarm   (swarm! ports config ctx scope))))

;; ---------------------------------------------------------------------------
;; Session hooks

(defn session-start
  "The :dirge/session-start hook: runs hive's catchup when the domain says
   so and answers {:context text} or nil."
  [ports config ctx]
  (when-let [req (d/session-start-request config ctx)]
    (d/session-start-context config (call! ports req))))

(defn session-end
  "The :dirge/session-end hook: records a hive wrap when the domain says so.
   dirge ignores the return value; the mcp-call answer is returned for tests."
  [ports config ctx]
  (when-let [req (d/session-end-request config ctx)]
    (call! ports req)))

;; ---------------------------------------------------------------------------
;; Tools

(defn tool-defs
  "Addon tool definitions proxying hive MCP tools through `ports`.
   `config-fn` answers the current configuration at call time."
  [ports config-fn]
  [{:name        "hive_memory_search"
    :description "Semantic search over hive memory (decisions, conventions, notes) for this project."
    :inputSchema {:type "object"
                  :properties {:query {:type "string" :description "natural-language query"}
                               :limit {:type "integer" :description "maximum results"}
                               :type  {:type "string" :description "entry type filter, e.g. decision"}}
                  :required ["query"]}
    :handler     (fn [params]
                   (d/tool-answer
                    (call! ports (d/memory-search-request (config-fn) ((:cwd ports)) params))))}
   {:name        "hive_kanban_list"
    :description "List hive kanban tasks for this project, optionally by status."
    :inputSchema {:type "object"
                  :properties {:status {:type "string"
                                        :enum ["todo" "inprogress" "inreview" "done"]}}}
    :handler     (fn [params]
                   (d/tool-answer
                    (call! ports (d/kanban-request (config-fn) ((:cwd ports)) (:status params)))))}])

;; ---------------------------------------------------------------------------
;; IAddon

(defn- current-config
  [state]
  (or (:config @state) (d/resolve-config nil)))

(defrecord HiveDirgeAddon [state ports]
  p/IAddon
  (addon-id [_] addon-id-str)
  (addon-type [_] :native)
  (capabilities [_] #{:tools :health-reporting :dirge/hooks :dirge/commands :dirge/panels})
  (initialize! [_ config]
    (reset! state {:config (d/resolve-config config) :initialized? true})
    {:success? true :errors [] :metadata {:addon/id addon-id-str}})
  (shutdown! [_]
    (swap! state assoc :initialized? false)
    {:success? true :errors []})
  (tools [_]
    (tool-defs ports #(current-config state)))
  (schema-extensions [_] {})
  (health [_]
    (let [s @state]
      (if (:initialized? s)
        {:status :ok :details {:server (get-in s [:config :hive/server])}}
        {:status :down :details {:reason :not-initialized}})))
  (excluded-tools [_] #{})
  (hooks [_]
    {:dirge/system-prompt (fn [_] (d/system-prompt (current-config state)))
     :dirge/session-start (fn [ctx] (session-start ports (current-config state) ctx))
     :dirge/session-end   (fn [ctx] (session-end ports (current-config state) ctx))
     :dirge/commands      {"hive" {:description "hive: catchup | wrap | kanban [status] | swarm [scope]"
                                   :handler     (fn [ctx]
                                                  (run-command ports (current-config state) ctx))}}}))

(defn make-addon
  "An uninitialized addon over `ports`."
  [ports]
  (->HiveDirgeAddon (atom {:config nil :initialized? false}) ports))

(defn addon-ctor
  "Manifest :addon/init-fn: config -> uninitialized IAddon bound to
   dirge.harness. Config is applied by initialize!."
  [_config]
  (make-addon harness-ports))
