(ns hive-dirge.lens.registry
  "Lens registry: the open set behind /hive <lens>. A lens is data plus pure
   fns over ports; adding a view is a registration, never a case edit (OCP).

   A lens:

     {:lens/id    keyword                 ; registry key and /hive argument
      :lens/title string
      :lens/panel string                 ; the show-panel id, == (name :lens/id)
      :lens/open  (fn [ports config ctx] -> {:fx [...] :text string}) ; the Boundary
      :lens/verbs {verb (fn [row payload] -> {:fx [...]})}            ; optional
      :lens/keys  {chord verb-or-{\"invoke\" verb}}                   ; optional
      :lens/cursor? bool}

   - :lens/open is a pull: it performs the effects the lens needs through
     PORTS (mcp-call and friends) and returns the show ops plus the chat
     text. It is the only impure surface and never leaves the addon layer.
   - :lens/verbs handle dirge invoke replies routed by panel id; row ids are
     the ids the lens stamped on its rendered lines.
   - :lens/keys use dirge chord syntax (\"enter\", \"tab\", \"shift-tab\"); a
     value is a verb string or {\"invoke\" verb}. Chords bind to the panel,
     and dirge fills the cursor row when one fires.
   - The panel op carries \"keys\" and \"cursor\": true whenever the lens
     declares :lens/keys, so dirge binds the chords and tracks a cursor.

   The wire contract (do not change): show-panel lines may be
   {face, spans:[{text,face}], id}; the invoke reply is
   {\"action\":\"invoke\",\"panel\":\"<panel id>\",\"verb\":\"<verb>\",\"row\": \"<row id or null>\",\"payload\":{...}};
   the feed op \"open-file\" takes {path,line?,diff?}.

   The registry itself is pure data: a function holding a map. Registration
   and lookup take the registry as the first argument, so any composition of
   registries is just map merging, and the namespace loads unchanged under
   cljrs and JVM Clojure. Effects stay behind the lens fns' ports argument
   (CPPB: this namespace is the Pipeline; ports are the Boundary)."

  (:require [clojure.string :as str]
            [hive-dirge.hive.domain :as domain]))

;; =============================================================================
;; Registry: data + pure fns
;; =============================================================================

(defn make-registry
  "A lens registry out of LENSES (a seq of lens maps). Later duplicates of
   the same :lens/id win, so (merge (make-registry a) (make-registry b))
   overlays b onto a. Returns a function of no arguments answering the
   {lens/id lens} map: a value in a var, resolved at call time (the rebind
   seam), never a global mutable registry."
  [lenses]
  (let [m (->> lenses
               (filter map?)
               (filter :lens/id)
               (map (fn [l] [(:lens/id l) l]))
               (into {}))]
    (fn [] m)))

(defn register-lens
  "REGISTRY with LENS added (its :lens/id replaces any previous entry)."
  [registry lens]
  {:pre [(map? lens) (some? (:lens/id lens))]}
  (make-registry (vals (assoc (registry) (:lens/id lens) lens))))

(defn lookup
  "The lens registered under ID, or nil. Strings (the /hive argv) are
   accepted alongside keywords."
  [registry id]
  (when-let [k (cond
                 (keyword? id) id
                 (string? id)  (keyword id)
                 :else nil)]
    (get (registry) k)))

(defn list-lenses
  "Every registered lens, sorted by :lens/id."
  [registry]
  (sort-by :lens/id (vals (registry))))

(defn known-verb?
  "True when LENS owns VERB (one of its :lens/verbs)."
  [lens verb]
  (contains? (set (keys (:lens/verbs lens))) verb))

(defn owner-of
  "The lens whose :lens/panel names PANEL, or nil."
  [registry panel]
  (some (fn [l] (when (= panel (:lens/panel l)) l)) (vals (registry))))

;; =============================================================================
;; Commands: opening a lens, listing the registry, routing an invoke
;; =============================================================================

(defn parse-open
  "The /hive lens invocation as data. Without a LENS the registry itself is
   listed; an unknown LENS answers :lens/unknown with the available ids."
  [registry lens]
  (if (nil? lens)
    {:command :lens/list}
    (if-let [l (lookup registry lens)]
      {:command :lens/open :lens l}
      {:command :lens/unknown :input lens
       :available (mapv :lens/id (list-lenses registry))})))

(defn- detail
  [l]
  (str "  /hive " (name (:lens/id l)) " — " (:lens/title l)))

(defn list-usage
  "The /hive chat text listing every registered lens."
  [registry]
  (str "hive lenses:\n" (str/join "\n" (map detail (list-lenses registry)))))

(defn- keys-payload
  "The \"keys\" and \"cursor\" fields a panel op carries for LENS: the lens's
   chords and cursor flag, or nothing when the lens declares no chords."
  [l]
  (when (seq (:lens/keys l))
    {:keys (:lens/keys l)
     :cursor (boolean (:lens/cursor? l))}))

(defn panel-op
  "The ui/show-panel op for lens L showing DOC. DOC carries ordinary vessel
   blocks plus optional :lens/rows (id-bearing lines); the dirge translator
   uses those rows to preserve ids in the wire lines."
  [l doc]
  (cond-> {:op          :ui/show-panel
           :panel/id    (:lens/panel l)
           :panel/title (:lens/title l)
           :doc         (dissoc doc :lens/rows)
           :panel/rows (vec (or (:lens/rows doc) []))}
    (seq (:lens/keys l)) (merge (keys-payload l))))

(defn open!
  "Open LENS through PORTS: the lens's :lens/open pull runs and its answer is
   normalized to {:fx [...] :text string}. A lens without :lens/open opens
   as a titled empty doc (a static lens)."
  [ports config ctx l]
  (if-let [open (:lens/open l)]
    (let [out (open ports config ctx)]
      {:fx   (vec (or (:fx out) []))
       :text (or (:text out) (str "lens " (:lens/id l) " opened"))})
    {:fx   [(panel-op l {:doc/title  (:lens/title l)
                         :doc/blocks []})]
     :text (str "lens " (:lens/id l) " opened")}))

;; =============================================================================
;; Invoke routing (H4)
;; =============================================================================

(defn route-invoke
  "The dirge invoke reply routed to a lens verb, as data. PANEL is the
   panel id, VERB the verb, ROW the cursor row id or nil, PAYLOAD the JSON
   object. Returns one of:

     {:invoke/routed lens-id verb row effects}   — run (fn [ports] effects)
     {:invoke/unknown-panel panel verb}          — ignore, warn
     {:invoke/unknown-verb panel verb}           — ignore, warn"
  [registry {:keys [panel verb row payload] :or {payload {}}}]
  (if-let [l (owner-of registry panel)]
    (if-let [handler (get (:lens/verbs l) verb)]
      {:invoke/routed (:lens/id l) :verb verb :row row
       :effects (fn [ports] (handler ports row payload))}
      {:invoke/unknown-verb verb :verb verb :panel panel :panel-owner (:lens/id l)})
    {:invoke/unknown-panel panel :panel panel :verb verb}))

;; =============================================================================
;; Capabilities derivation (feeds the C3 discovery doc)
;; =============================================================================

(defn all-verbs
  "Every verb owned by any lens in REGISTRY, sorted, as strings."
  [registry]
  (->> (vals (registry))
       (mapcat #(keys (:lens/verbs %)))
       (distinct)
       (sort)
       vec))

(defn merged-keys
  "The key chords of every lens merged into one map, string chords. A chord
   bound by two lenses keeps the first binding; each panel carries its own
   authoritative :keys, so discovery is only a global capability hint."
  [registry]
  (->> (list-lenses registry)
       (map :lens/keys)
       (filter map?)
       (reduce (fn [acc ks]
                 (reduce-kv (fn [m chord binding]
                              (if (contains? m chord) m (assoc m chord binding))) acc ks)) {})))

(defn capabilities-fragment
  "The pure derivation of the C3 capabilities fragment: {\"invokes\" [...]
   every lens verb ...], \"keys\" {... every lens chord ...}}. The host
   merges this into the discovery document's \"capabilities\" map; dirge's
   local grid actions are not bindable from here."
  [registry]
  {"invokes" (all-verbs registry)
   "keys"    (merged-keys registry)})

;; =============================================================================
;; Built-in lenses: the kanban and swarm views, ported off the closed
;; parse-command / run-command cases.
;; =============================================================================

(defn- builtin-open
  "The :lens/open of a pull lens that delegates to the domain panel builders.
   PULL performs the mcp-call through PORTS. PANEL receives the parsed DATA
   and the raw TEXT and shapes the dirge show op; SUMMARY receives DATA and
   answers the chat text. Both results get the lens's key chords and cursor
   flag merged in."
  [ports config ctx {:keys [pull panel summary lens]}]
  (let [answer (pull ports config ctx)
        text   (domain/answer-body (domain/result-text answer))]
    (if-let [err (domain/result-error answer)]
      {:text (str "/hive " (name (:lens/id lens)) " failed: " err)}
      (let [data ((:json-parse ports) text)
            p    (panel data text)
            doc  (if (:markdown p)
                   {:doc/title  (:title p)
                    :doc/blocks [{:block/type :code :text (:markdown p)}]}
                   {:doc/title  (:title p)
                    :doc/blocks (mapv (fn [line] {:block/type :para :text (:text line)})
                                      (:lines p))
                    :lens/rows (:lines p)})]
        {:fx   [(panel-op lens doc)]
         :text (summary data)}))))

(def kanban-lens
  "The project kanban as a lens: pulls the kanban list over the hive MCP
   connection, one line per task with the task id as its row id."
  {:lens/id      :kanban
   :lens/title   "Kanban"
   :lens/panel   "kanban"
   :lens/cursor? true
   :lens/keys    {"enter" {"invoke" "open"}}
   :lens/open
   (fn [ports config ctx]
     (let [status (:lens/status ctx)
           dir    (or (:cwd ctx) ((:cwd ports)))
           lens   kanban-lens]
       (builtin-open ports config ctx
                     {:lens   lens
                      :pull   (fn [ports _ _]
                                ((:mcp-call ports) (:hive/server config) "project"
                                 (nth (domain/kanban-request config dir status) 2)))
                      :panel  (fn [data text]
                                (domain/kanban-panel (domain/kanban-rows data)
                                                     text (domain/project-hint dir) status))
                      :summary (fn [data]
                                 (domain/kanban-summary (domain/kanban-rows data)
                                                        (domain/project-hint dir)))})))
   :lens/verbs
   {"open" (fn [ports row _payload]
             (when (and row (:log! ports))
               ((:log! ports) :info (str "kanban open " row))))}})

(def swarm-lens
  "The hive agents as a lens: pulls the agent registry over the hive MCP
   connection, working agents first, one line per agent with the agent id as
   its row id."
  {:lens/id      :swarm
   :lens/title   "Swarm"
   :lens/panel   "swarm"
   :lens/cursor? true
   :lens/keys    {"enter" {"invoke" "focus"}}
   :lens/open
   (fn [ports config ctx]
     (let [scope (:lens/scope ctx)
           dir   (or (:cwd ctx) ((:cwd ports)))
           lens  swarm-lens]
       (builtin-open ports config ctx
                     {:lens   lens
                      :pull   (fn [ports _ _]
                                ((:mcp-call ports) (:hive/server config) "swarm"
                                 (nth (domain/swarm-request config dir scope) 2)))
                      :panel  (fn [data text]
                                (domain/swarm-panel (domain/swarm-rows data) text scope))
                      :summary (fn [data]
                                 (domain/swarm-summary (domain/swarm-rows data)))})))
   :lens/verbs
   {"focus" (fn [ports row _payload]
              (when-let [log! (:log! ports)]
                (log! :info (str "swarm focus " row))))}})

(def builtins
  "The lenses shipped with the registry: its first entries."
  [kanban-lens swarm-lens])

(defn builtin-registry
  "The registry holding just the built-in lenses."
  []
  (make-registry builtins))

(defn with-builtins
  "REGISTRY with every lens of EXTRA merged in (extra wins on :lens/id).
   Other IAddons compose their lenses in through this overlay; no addon ever
   edits another addon's core."
  [registry extra]
  (make-registry (concat (vals (registry)) extra)))
