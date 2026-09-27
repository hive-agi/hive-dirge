(ns hive-dirge.hive.domain
  "Pure domain of the hive.dirge addon: configuration, /hive command parsing,
   hive MCP request construction and MCP result shaping. No effects, no
   dirge.harness: every function here is data in, data out, and loads
   unchanged under cljrs and JVM Clojure."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Configuration

(def default-config
  "Configuration keys the addon reads, with their defaults."
  {:hive/server            "hive"
   :hive/kanban-limit      50
   :hive/max-prompt-chars  60000
   :hive/memory-limit      10
   :hive/swarm-scope       :all})

(defn- positive-int?
  [x]
  (and (integer? x) (pos? x)))

(defn resolve-swarm-scope
  "The swarm listing scope out of a configured value: :all (every agent),
   :project (agents of the current project) or a project id string. Keywords
   and their string names are both accepted; anything else is :all."
  [v]
  (cond
    (#{:all "all"} v)         :all
    (#{:project "project"} v) :project
    (and (string? v) (not (str/blank? v))) v
    :else (:hive/swarm-scope default-config)))

(defn resolve-config
  "Effective configuration from what a host hands the addon. Accepts the
   dirge initialize! shape {:addon/config {...}} or a bare config map; keys
   absent or invalid fall back to `default-config`."
  [raw]
  (let [cfg (merge (when (map? raw) (dissoc raw :addon/config))
                   (when (map? raw) (:addon/config raw)))
        server (:hive/server cfg)]
    {:hive/server           (if (and (string? server) (not (str/blank? server)))
                              server
                              (:hive/server default-config))
     :hive/kanban-limit     (let [n (:hive/kanban-limit cfg)]
                              (if (positive-int? n) n (:hive/kanban-limit default-config)))
     :hive/max-prompt-chars (let [n (:hive/max-prompt-chars cfg)]
                              (if (positive-int? n) n (:hive/max-prompt-chars default-config)))
     :hive/memory-limit     (let [n (:hive/memory-limit cfg)]
                              (if (positive-int? n) n (:hive/memory-limit default-config)))
     :hive/swarm-scope      (resolve-swarm-scope (:hive/swarm-scope cfg))}))

;; ---------------------------------------------------------------------------
;; /hive command parsing

(def kanban-statuses
  "Statuses the hive kanban accepts as a list filter."
  #{"todo" "inprogress" "inreview" "done"})

(def usage
  "Help text for the /hive command family."
  (str "/hive catchup          load hive session context (memory, kanban, git) into the next turn\n"
       "/hive wrap             record a hive session wrap for this project\n"
       "/hive kanban [status]  show the project kanban in a side panel (status: todo, inprogress, inreview, done)\n"
       "/hive swarm [scope]    show hive agents in a side panel (scope: all, project, or a project id)"))

(defn- words
  [s]
  (if (string? s)
    (vec (remove str/blank? (str/split (str/trim s) #"\s+")))
    []))

(defn parse-command
  "The /hive invocation as an action map. `ctx` is dirge's command context
   {:args :argv :cwd}; :argv wins over :args when both are present.
   Answers {:action :catchup|:wrap|:kanban|:help|:unknown ...}."
  [ctx]
  (let [argv (let [v (:argv ctx)]
               (if (and (sequential? v) (seq v)) (vec (map str v)) (words (:args ctx))))
        [sub & more] argv]
    (case sub
      nil       {:action :help}
      "help"    {:action :help}
      "catchup" {:action :catchup}
      "wrap"    {:action :wrap}
      "kanban"  (let [status (first more)]
                  (cond
                    (nil? status)                   {:action :kanban}
                    (contains? kanban-statuses status) {:action :kanban :status status}
                    :else {:action :unknown
                           :reason (str "unknown kanban status: " status)}))
      "swarm"   (if-let [scope (first more)]
                  {:action :swarm :scope (resolve-swarm-scope scope)}
                  {:action :swarm})
      {:action :unknown :reason (str "unknown /hive subcommand: " sub)})))

;; ---------------------------------------------------------------------------
;; Requests: [server tool args] triples for dirge.harness/mcp-call

(defn project-hint
  "The last path segment of `directory`, which is how hive names a project
   by default."
  [directory]
  (when (string? directory)
    (last (remove str/blank? (str/split directory #"/")))))

(defn- with-directory
  [args directory]
  (if (and (string? directory) (not (str/blank? directory)))
    (assoc args "directory" directory)
    args))

(defn catchup-request
  "mcp-call triple for hive's workflow catchup in `directory`."
  [config directory]
  [(:hive/server config) "project"
   (with-directory {"command" "workflow catchup"} directory)])

(defn wrap-request
  "mcp-call triple for hive's session wrap in `directory`."
  [config directory]
  [(:hive/server config) "project"
   (with-directory {"command" "session wrap"} directory)])

(defn kanban-request
  "mcp-call triple for hive's kanban list in `directory`, optionally narrowed
   to `status` (one of `kanban-statuses`)."
  [config directory status]
  [(:hive/server config) "project"
   (cond-> (with-directory {"command" "kanban list"
                            "limit"   (:hive/kanban-limit config)}
             directory)
     (contains? kanban-statuses status) (assoc "status" status))])

(defn memory-search-request
  "mcp-call triple for hive's semantic memory search. `params` carries
   :query (required), optional :limit and :type."
  [config directory params]
  (let [limit (:limit params)
        type  (:type params)]
    [(:hive/server config) "memory"
     (cond-> (with-directory {"command" "search"
                              "query"   (str (:query params))
                              "limit"   (if (positive-int? limit)
                                          limit
                                          (:hive/memory-limit config))}
               directory)
       (and (string? type) (not (str/blank? type))) (assoc "type" type))]))

(def swarm-all-agent-id
  "agent_id that makes hive's agent status list the whole registry instead
   of the caller's own row (a caller id is otherwise injected per session)."
  "coordinator")

(defn swarm-project
  "The project id a swarm `scope` narrows to in `directory`, or nil for
   :all."
  [scope directory]
  (cond
    (= :all scope)     nil
    (= :project scope) (project-hint directory)
    (string? scope)    scope
    :else nil))

(defn swarm-request
  "mcp-call triple for hive's agent status listing under `scope` (nil means
   the configured :hive/swarm-scope). project_id is sent only when the
   scope narrows to a project: any project_id, even the session's own,
   filters the registry by it."
  [config directory scope]
  (let [project (swarm-project (or scope (:hive/swarm-scope config)) directory)]
    [(:hive/server config) "swarm"
     (cond-> {"command" "agent status" "agent_id" swarm-all-agent-id}
       project (assoc "project_id" project))]))

;; ---------------------------------------------------------------------------
;; MCP result shaping

(defn result-error
  "The error message carried by an mcp-call answer, or nil when it
   succeeded. A nil answer (no dirge harness) is an error too."
  [answer]
  (cond
    (nil? answer)       "hive MCP is unreachable: not running inside dirge"
    (not (map? answer)) (str "unexpected MCP answer: " (pr-str answer))
    (:error answer)     (str (:error answer))
    (:isError answer)   (let [t (str/join "\n" (keep :text (:content answer)))]
                          (if (str/blank? t) "hive MCP tool reported an error" t))
    :else nil))

(defn result-text
  "The text blocks of an MCP tool result joined with newlines."
  [answer]
  (str/join "\n" (keep (fn [c] (when (string? (:text c)) (:text c)))
                       (:content answer))))

(defn truncate
  "`s` cut to at most `n` characters, with a marker saying how much was
   dropped."
  [s n]
  (if (<= (count s) n)
    s
    (str (subs s 0 n) "\n[... " (- (count s) n) " characters truncated]")))

(defn catchup-prompt
  "The prompt that hands a catchup result to the model."
  [config text]
  (str "Hive session context for this project, loaded by /hive catchup. "
       "Read it, keep its axioms and conventions in force, and reply with a short "
       "summary of the open work and relevant decisions.\n\n"
       (truncate text (:hive/max-prompt-chars config))))

(defn command-reply
  "dirge command reply for an mcp-call `answer` of `action`. Pure: kanban
   rows arrive already parsed as `rows` (nil when unparsable)."
  [config action answer]
  (if-let [err (result-error answer)]
    {:text (str "/hive " (name action) " failed: " err)}
    (let [text (result-text answer)]
      (case action
        :catchup {:text   (str "hive catchup loaded (" (count text) " characters)")
                  :prompt (catchup-prompt config text)}
        :wrap    {:text (if (str/blank? text) "hive session wrapped" text)}
        {:text text}))))

;; ---------------------------------------------------------------------------
;; Kanban rows -> side panel

(defn kanban-rows
  "Task rows out of a parsed kanban list answer: a vector of rows, or a map
   holding them under :tasks, :items or :results. Anything else is nil."
  [data]
  (cond
    (sequential? data) (vec (filter map? data))
    (map? data)        (some (fn [k] (when (sequential? (get data k))
                                       (vec (filter map? (get data k)))))
                             [:tasks :items :results])
    :else nil))

(defn local-first
  "`rows` with the ones belonging to `project` first, order otherwise kept."
  [rows project]
  (let [local? #(= project (:project %))]
    (into (vec (filter local? rows)) (remove local? rows))))

(defn- priority-face
  [priority]
  (case (str priority)
    "high" "warn"
    "low"  "dim"
    "normal"))

(defn kanban-line
  "One panel line for a kanban row."
  [row]
  {:text (str "[" (or (:status row) "?") "] "
              (or (:title row) "(untitled)")
              "  " (:id row))
   :face (priority-face (:priority row))})

(defn kanban-panel
  "A dirge panel :show op listing `rows`, rows of `project` first. With nil
   rows (unparsable answer) the raw `text` is shown as markdown."
  [rows text project status]
  (let [title (str "hive kanban" (when status (str " (" status ")")))]
    (if (nil? rows)
      {:op :show :id "hive-kanban" :title title :markdown text}
      {:op :show :id "hive-kanban" :title title
       :lines (if (seq rows)
                (mapv kanban-line (local-first rows project))
                [{:text "no tasks" :face "dim"}])})))

(defn kanban-summary
  "The chat line after the kanban panel is shown."
  [rows project]
  (if (nil? rows)
    "hive kanban shown in the side panel"
    (let [local (count (filter #(= project (:project %)) rows))]
      (str "hive kanban: " (count rows) " tasks"
           (when (pos? local) (str ", " local " in " project))
           " (side panel)"))))

;; ---------------------------------------------------------------------------
;; Swarm agents -> side panel

(defn swarm-rows
  "Agent rows out of a parsed agent status answer: {:agents [...]}, a single
   {:agent {...}} or a bare vector. Anything else is nil."
  [data]
  (cond
    (sequential? data)              (vec (filter map? data))
    (sequential? (:agents data))    (vec (filter map? (:agents data)))
    (map? (:agent data))            [(:agent data)]
    :else nil))

(defn- agent-project
  [row]
  (or (:project-id row) (:project_id row) (:project row)))

(defn- status-face
  [status]
  (case (str status)
    "working" "normal"
    "idle"    "dim"
    "blocked" "warn"
    "error"   "warn"
    "normal"))

(defn swarm-line
  "One panel line for an agent row: id, status, project."
  [row]
  {:text (str (or (:id row) "?") "  " (or (:status row) "?") "  "
              (or (agent-project row) "-"))
   :face (status-face (:status row))})

(defn swarm-title
  [scope]
  (str "hive swarm"
       (cond
         (= :all scope)     " (all)"
         (= :project scope) " (project)"
         (string? scope)    (str " (" scope ")")
         :else "")))

(defn swarm-panel
  "A dirge panel :show op listing agent `rows`. With nil rows (unparsable
   answer) the raw `text` is shown as markdown."
  [rows text scope]
  (if (nil? rows)
    {:op :show :id "hive-swarm" :title (swarm-title scope) :markdown text}
    {:op :show :id "hive-swarm" :title (swarm-title scope)
     :lines (if (seq rows)
              (mapv swarm-line rows)
              [{:text "no agents" :face "dim"}])}))

(defn swarm-summary
  "The chat line after the swarm panel is shown."
  [rows]
  (if (nil? rows)
    "hive swarm shown in the side panel"
    (let [working (count (filter #(= "working" (:status %)) rows))]
      (str "hive swarm: " (count rows) " agents"
           (when (pos? working) (str ", " working " working"))
           " (side panel)"))))

;; ---------------------------------------------------------------------------
;; Addon tool answers and the system prompt

(defn tool-answer
  "An addon tool result for an mcp-call `answer`: the MCP result itself when
   it succeeded, an :isError text result otherwise."
  [answer]
  (if-let [err (result-error answer)]
    {:content [{:type "text" :text err}] :isError true}
    {:content (vec (:content answer)) :isError false}))

(defn system-prompt
  "Static system-prompt text announcing the /hive commands and tools."
  [config]
  (str "The hive.dirge addon connects this session to hive (MCP server \""
       (:hive/server config) "\"). The user can run /hive catchup (load hive "
       "memory and kanban context), /hive wrap (record a session wrap) and "
       "/hive kanban [status] (show tasks in the side panel) and /hive swarm "
       "[scope] (show hive agents in the side panel). You can call the "
       "hive_memory_search and hive_kanban_list tools to consult hive memory "
       "and the project kanban."))
