(ns hive-dirge.hive.addon
  "hive.dirge: the in-dirge IAddon that connects a dirge session to hive over
   dirge's own MCP connection. Contributes the /hive command family
   (catchup, wrap, kanban, memory, swarm, shout), two tools proxying hive
   memory search and the kanban list, a static system-prompt hook, and the
   session hooks that run catchup at session start and wrap at session end.

   Effects go through a ports map {:mcp-call :json-parse :panel! :cwd} so the
   pipeline is testable with plain fns; `harness-ports` binds them to
   dirge.harness, which makes every effect a no-op outside dirge."
  (:require [hive-addon.protocol :as p]
            [hive-dirge.harness :as h]
            [hive-dirge.hive.domain :as d]
            [hive-dirge.lens.registry :as lens]))

(def addon-id-str "hive.dirge")

(def harness-ports
  "Ports bound to the dirge.harness natives."
  {:mcp-call   h/mcp-call
   :json-parse h/json-parse
   :panel!     h/panel!
   :cwd        h/cwd
   :log!       h/log!
   :feed!      h/panel!})

(defn- call!
  [ports [server tool args]]
  ((:mcp-call ports) server tool args))

(defn- directory
  [ports ctx]
  (or (:cwd ctx) ((:cwd ports))))

;; ---------------------------------------------------------------------------
;; /hive command pipeline

(defn- body
  "The answer text of a successful call, context blocks cut off."
  [answer]
  (d/answer-body (d/result-text answer)))

(defn- memory!
  [ports config ctx query]
  (let [answer (call! ports (d/memory-search-request config (directory ports ctx) {:query query}))]
    (if-let [err (d/result-error answer)]
      {:text (str "/hive memory failed: " err)}
      {:text (d/memory-text query ((:json-parse ports) (body answer)))})))

(defn- lens!
  "The /hive lens command: no name lists the registry, an unknown one lists
   it too (with the bad name called out), a known one opens it, delivering
   every fx through the panel port."
  [registry ports config ctx]
  (let [{:keys [status scope lens]} (d/parse-command ctx)
        open  (lens/parse-open registry lens)
        ctx   (cond-> ctx
                status (assoc :lens/status status)
                scope  (assoc :lens/scope scope))
        emit! (fn [fx] (run! #((:panel! ports) %) fx))]
    (case (:command open)
      :lens/list     {:text (lens/list-usage registry)}
      :lens/unknown  {:text (str "unknown lens: " (:input open) "\n"
                                 (lens/list-usage registry))}
      :lens/open     (let [out (lens/open! ports config ctx (:lens open))]
                       (emit! (:fx out))
                       {:text (:text out)}))))

(defn- invoke!
  "The /hive invoke <panel> <verb> <row?> command: routes the invoke to the
   owning lens's verb through the registry, warns and ignores an unknown
   panel or verb."
  [registry ports panel verb row]
  (let [routed (lens/route-invoke registry {:panel panel :verb verb :row row :payload {}})]
    (if (:invoke/routed routed)
      (do ((:effects routed) ports) {:text (str panel ": " verb)})
      (do (when-let [log! (:log! ports)]
            (log! :warn (str "hive invoke ignored: " (:verb routed)
                             " on " (:panel routed))))
          {:text (str "ignored: " (:verb routed) " on " (:panel routed))}))))

(defn- shout!
  [ports config ctx message]
  (let [answer (call! ports (d/shout-request config (directory ports ctx) message))]
    (if-let [err (d/result-error answer)]
      {:text (str "/hive shout failed: " err)}
      {:text "shouted to the hivemind"})))

(defn run-command
  "Handles one /hive invocation against `ports` and the lens REGISTRY
   (default: the built-in lenses). Answers a dirge command reply {:text ..}
   or {:text .. :prompt ..}."
  ([ports config ctx] (run-command (lens/builtin-registry) ports config ctx))
  ([registry ports config ctx]
   (let [{:keys [action status scope reason query message lens panel verb row]}
         (d/parse-command ctx)]
     (case action
       :help    {:text d/usage}
       :unknown {:text (str reason "\n" d/usage)}
       :catchup (d/command-reply config :catchup
                                 (call! ports (d/catchup-request config (directory ports ctx))))
       :wrap    (d/command-reply config :wrap
                                 (call! ports (d/wrap-request config (directory ports ctx))))
       :lens    (lens! registry ports config ctx)
       :invoke  (invoke! registry ports panel verb row)
       :memory  (memory! ports config ctx query)
       :shout   (shout! ports config ctx message)))))

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
;; Command hooks

(defn guard-hook
  "The \"guard\" command hook (`{\"type\": \"addon\", \"addon\": \"hive.dirge\",
   \"handler\": \"guard\"}` in a dirge hooks block): the hook payload in `ctx`
   judged by hive's guard over MCP. Answers the Claude-style hook JSON dirge
   reads as the hook's stdout. FAIL-OPEN: any way of getting no verdict
   answers {} (the action allowed) and logs why. That includes hooks dirge
   runs on its event loop, where an MCP call is refused at once."
  [ports config ctx]
  (let [{:keys [answer gap]}
        (try
          (d/guard-answer (call! ports (d/guard-request config (:payload ctx)))
                          (:json-parse ports))
          (catch #?(:clj Throwable :default :default) t
            {:answer {} :gap (str "guard hook failed: " (or (ex-message t) t))}))]
    (when-let [log! (and gap (:log! ports))]
      (log! :warn (str "hive guard: allowed unjudged, " gap)))
    answer))

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

(defrecord HiveDirgeAddon [state ports registry]
  p/IAddon
  (addon-id [_] addon-id-str)
  (addon-type [_] :native)
  (capabilities [_] #{:tools :health-reporting :dirge/hooks :dirge/commands :dirge/command-hooks :dirge/panels})
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
     :dirge/commands      {"hive" {:description "hive: catchup | wrap | kanban [status] | swarm [scope] | lens [name] | memory <query> | shout <message>"
                                   :handler     (fn [ctx]
                                                  (run-command registry ports (current-config state) ctx))}}
     :dirge/invoke        (fn [{:keys [panel verb row payload] :or {payload {}}}]
                            ;; The in-dirge half of invoke routing: the lens
                            ;; registry owns the panel ids, so the verb runs
                            ;; here. A reply the host already routed (panel
                            ;; owned there) never reaches this hook. Unknown
                            ;; panel or verb: warn and ignore.
                            (let [routed (lens/route-invoke registry {:panel panel
                                                                      :verb verb
                                                                      :row row
                                                                      :payload payload})]
                              (if (:invoke/routed routed)
                                ((:effects routed) ports)
                                (when-let [log! (:log! ports)]
                                  (log! :warn (str "hive invoke ignored: " verb
                                                   " on " panel))))))
     :dirge/command-hooks {"guard" (fn [ctx] (guard-hook ports (current-config state) ctx))}}))

(defn make-addon
  "An uninitialized addon over `ports`. REGISTRY defaults to the built-in
   lenses; hosts compose extra lenses with
   hive-dirge.lens.registry/with-builtins."
  ([ports] (make-addon ports (lens/builtin-registry)))
  ([ports registry]
   (->HiveDirgeAddon (atom {:config nil :initialized? false}) ports registry)))

(defn addon-ctor
  "Manifest :addon/init-fn: config -> uninitialized IAddon bound to
   dirge.harness. Config is applied by initialize!."
  [_config]
  (make-addon harness-ports))
