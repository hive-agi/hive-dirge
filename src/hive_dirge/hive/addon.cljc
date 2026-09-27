(ns hive-dirge.hive.addon
  "hive.dirge: the in-dirge IAddon that connects a dirge session to hive over
   dirge's own MCP connection. Contributes the /hive command family
   (catchup, wrap, kanban, memory, swarm, shout), two tools proxying hive
   memory search and the kanban list, and a static system-prompt hook.

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

(defn- parsed
  "The JSON answer of a successful call as data, context blocks cut off."
  [ports answer]
  ((:json-parse ports) (d/answer-body (d/result-text answer))))

(defn- kanban!
  [ports config ctx status]
  (let [dir    (directory ports ctx)
        answer (call! ports (d/kanban-request config dir status))]
    (if-let [err (d/result-error answer)]
      {:text (str "/hive kanban failed: " err)}
      (let [text    (d/answer-body (d/result-text answer))
            rows    (d/kanban-rows ((:json-parse ports) text))
            project (d/project-hint dir)]
        ((:panel! ports) (d/kanban-panel rows text project status))
        {:text (d/kanban-summary rows project)}))))

(defn- memory!
  [ports config ctx query]
  (let [answer (call! ports (d/memory-search-request config (directory ports ctx) {:query query}))]
    (if-let [err (d/result-error answer)]
      {:text (str "/hive memory failed: " err)}
      {:text (d/memory-text query (parsed ports answer))})))

(defn- swarm!
  [ports config]
  (let [answer (call! ports (d/swarm-request config))]
    (if-let [err (d/result-error answer)]
      {:text (str "/hive swarm failed: " err)}
      (let [agents (:agents (parsed ports answer))]
        ((:panel! ports) (d/swarm-panel agents))
        {:text (d/swarm-summary agents)}))))

(defn- shout!
  [ports config ctx message]
  (let [answer (call! ports (d/shout-request config (directory ports ctx) message))]
    (if-let [err (d/result-error answer)]
      {:text (str "/hive shout failed: " err)}
      {:text "shouted to the hivemind"})))

(defn run-command
  "Handles one /hive invocation against `ports`. Answers a dirge command
   reply {:text ..} or {:text .. :prompt ..}."
  [ports config ctx]
  (let [{:keys [action status reason query message]} (d/parse-command ctx)]
    (case action
      :help    {:text d/usage}
      :unknown {:text (str reason "\n" d/usage)}
      :catchup (d/command-reply config :catchup
                                (call! ports (d/catchup-request config (directory ports ctx))))
      :wrap    (d/command-reply config :wrap
                                (call! ports (d/wrap-request config (directory ports ctx))))
      :kanban  (kanban! ports config ctx status)
      :memory  (memory! ports config ctx query)
      :swarm   (swarm! ports config)
      :shout   (shout! ports config ctx message))))


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
     :dirge/commands      {"hive" {:description "hive: catchup | wrap | kanban [status] | memory <query> | swarm | shout <message>"
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
