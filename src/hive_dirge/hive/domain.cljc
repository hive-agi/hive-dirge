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
   :hive/memory-limit      10})

(defn- positive-int?
  [x]
  (and (integer? x) (pos? x)))

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
                              (if (positive-int? n) n (:hive/memory-limit default-config)))}))

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
       "/hive memory <query>   search hive memory; hits listed in the chat\n"
       "/hive swarm            every agent and its status, working ones first, in a side panel\n"
       "/hive shout <message>  post progress to the hivemind"))

(defn- words
  [s]
  (if (string? s)
    (vec (remove str/blank? (str/split (str/trim s) #"\s+")))
    []))

(defn parse-command
  "The /hive invocation as an action map. `ctx` is dirge's command context
   {:args :argv :cwd}; :argv wins over :args when both are present.
   Answers {:action :catchup|:wrap|:kanban|:memory|:swarm|:shout|:help|:unknown ...}."
  [ctx]
  (let [argv (let [v (:argv ctx)]
               (if (and (sequential? v) (seq v)) (vec (map str v)) (words (:args ctx))))
        [sub & more] argv
        text (str/join " " more)]
    (case sub
      nil       {:action :help}
      "help"    {:action :help}
      "catchup" {:action :catchup}
      "wrap"    {:action :wrap}
      "swarm"   {:action :swarm}
      "kanban"  (let [status (first more)]
                  (cond
                    (nil? status)                   {:action :kanban}
                    (contains? kanban-statuses status) {:action :kanban :status status}
                    :else {:action :unknown
                           :reason (str "unknown kanban status: " status)}))
      "memory"  (if (str/blank? text)
                  {:action :unknown :reason "usage: /hive memory <query>"}
                  {:action :memory :query text})
      "shout"   (if (str/blank? text)
                  {:action :unknown :reason "usage: /hive shout <message>"}
                  {:action :shout :message text})
      {:action :unknown :reason (str "unknown /hive subcommand: " sub)})))

;; ---------------------------------------------------------------------------
;; Requests: [server tool args] triples for dirge.harness/mcp-call

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

(defn swarm-request
  "mcp-call triple for hive's agent status across every project: no
   directory, which would narrow the swarm to one project."
  [config]
  [(:hive/server config) "swarm" {"command" "agent status" "verbosity" "slim"}])

(defn shout-request
  "mcp-call triple posting `message` to the hivemind as progress from dirge."
  [config directory message]
  [(:hive/server config) "swarm"
   (with-directory {"command"    "hivemind shout"
                    "event_type" "progress"
                    "task"       "dirge"
                    "message"    message}
     directory)])

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

(defn answer-body
  "The answer part of an MCP result text. hive appends context blocks
   (---MEMORY--- and the like) after the answer, which no JSON reader
   accepts; they are cut off here."
  [text]
  (let [text   (str text)
        marker (re-find #"\n+---[A-Z][A-Z-]*---" text)]
    (str/trim (if marker (subs text 0 (str/index-of text marker)) text))))

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

(defn project-hint
  "The last path segment of `directory`, which is how hive names a project
   by default."
  [directory]
  (when (string? directory)
    (last (remove str/blank? (str/split directory #"/")))))

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

(defn memory-text
  "Chat text for a parsed memory search answer {:results [{:id :type :title}]}."
  [query data]
  (let [hits (:results data)]
    (if (seq hits)
      (str/join "\n"
                (cons (str (count hits) " memories for \"" query "\"")
                      (map (fn [{:keys [id type title]}]
                             (str "  [" type "] " title "  (" id ")"))
                           hits)))
      (str "no memories for \"" query "\""))))

(def ^:private status-faces
  {"working" "success" "idle" "dim" "blocked" "warn" "error" "error"})

(defn swarm-panel
  "A dirge panel :show op listing swarm `agents` ({:id :status :project-id}),
   working ones first, each group by id."
  [agents]
  (let [working? #(= "working" (:status %))
        ordered  (concat (sort-by :id (filter working? agents))
                         (sort-by :id (remove working? agents)))]
    {:op :show :id "hive-swarm" :title "hive swarm"
     :lines (if (seq ordered)
              (mapv (fn [{:keys [id status project-id]}]
                      {:text (str id " " status (when project-id (str " " project-id)))
                       :face (get status-faces status "normal")})
                    ordered)
              [{:text "no agents" :face "dim"}])}))

(defn swarm-summary
  "The chat line after the swarm panel: how many agents per status."
  [agents]
  (let [by (frequencies (map :status agents))]
    (str "hive swarm: " (count agents) " agent(s)"
         (when (seq by)
           (str ", " (str/join ", " (map (fn [s] (str (get by s) " " s)) (sort (keys by))))))
         " (side panel)")))

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
       "memory and kanban context), /hive wrap (record a session wrap), "
       "/hive kanban [status] and /hive swarm (side panels), /hive memory <query> "
       "and /hive shout <message>, none of which spends a turn. You can call the "
       "hive_memory_search and hive_kanban_list tools to consult hive memory "
       "and the project kanban."))
