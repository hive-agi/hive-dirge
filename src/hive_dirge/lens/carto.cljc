(ns hive-dirge.lens.carto
  "Lens L1: the carto neighborhood of one qualified name. /hive carto <qn>
   pulls carto callers and callees of QN over the hive MCP connection and
   shows them as a layered DAG block (callers -> QN -> callees) plus one
   cursor row per node.

   Verbs (row = the node's qn, payload = {:qn :file :line}):
     recenter  enter  re-open the lens centred on the selected node
     open      o      open the node's definition (ui/open-file)

   Layers (CPPB): the request builders, `neighbors`, `neighborhood` and `doc`
   are the pure Pipeline; `open-lens` is the Boundary and reaches carto only
   through the PORTS map (:mcp-call :json-parse :panel! :cwd). Works for any
   language carto indexes, Rust included, so dirge itself is navigable."
  (:require [clojure.string :as str]
            [hive-dirge.hive.domain :as domain]
            [hive-dirge.lens.registry :as registry]))

;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Requests: [server tool args] triples for mcp-call
;; =============================================================================

(def default-depth 1)

(defn- carto-request
  [config command qn depth dir]
  [(:hive/server config) "code"
   (cond-> {"command" command "function" qn "depth" depth}
     dir (assoc "directory" dir))])

(defn callers-request
  "The carto callers call for QN."
  [config qn depth dir]
  (carto-request config "carto callers" qn depth dir))

(defn callees-request
  "The carto callees call for QN."
  [config qn depth dir]
  (carto-request config "carto callees" qn depth dir))

;; =============================================================================
;; Pure: answers -> neighbors -> nodes, edges, rows
;; =============================================================================

(defn neighbors
  "The neighbor entries of a parsed carto callers/callees answer under K
   (:callers or :callees): a vector of {:qn :file? :line?}. Entries without a
   qualified qn (carto's unresolved external ids) are dropped; duplicates
   collapse to their first occurrence."
  [data k]
  (let [entries (when (map? data) (get data k))]
    (->> (if (sequential? entries) entries [])
         (filter map?)
         (keep (fn [e]
                 (let [qn (:qn e)]
                   (when (and (string? qn) (str/includes? qn "/"))
                     (cond-> {:qn qn}
                       (:file e) (assoc :file (:file e))
                       (:line e) (assoc :line (:line e)))))))
         (reduce (fn [acc e] (if (some #(= (:qn e) (:qn %)) acc) acc (conj acc e))) [])
         vec)))

(defn- node-row
  [arrow face {:keys [qn file line] :as entry}]
  {:text    (str arrow " " qn (when file (str "  " file (when line (str ":" line)))))
   :face    face
   :id      qn
   :payload (select-keys entry [:qn :file :line])})

(defn neighborhood
  "The lens view of CENTER (a qn) with CALLERS and CALLEES ({:qn :file :line}
   vectors): DAG nodes and edges (caller -> center -> callee) and the cursor
   rows, callers first, then the center, then the callees. A node that is
   both caller and callee keeps both edges and one row per role."
  [center callers callees]
  {:nodes (mapv (fn [id] {:id id :label id})
                (distinct (concat (map :qn callers) [center] (map :qn callees))))
   :edges (vec (distinct (concat (map (fn [c] [(:qn c) center]) callers)
                                 (map (fn [c] [center (:qn c)]) callees))))
   :rows  (vec (concat (map (partial node-row "←" "dim") callers)
                       [(node-row "●" "title" {:qn center})]
                       (map (partial node-row "→" "normal") callees)))})

(defn doc
  "The show-panel doc of neighborhood N around CENTER: a summary line and
   the DAG block, with N's rows as :lens/rows (the host renders the blocks
   above the cursor rows)."
  [center {:keys [nodes edges rows]} n-callers n-callees]
  {:doc/title  (str "carto " center)
   :doc/blocks [{:block/type :para
                 :text (str n-callers " callers, " n-callees " callees")}
                {:block/type :dag :nodes nodes :edges edges}]
   :lens/rows  rows})

(defn summary
  [center n-callers n-callees]
  (str "carto " center ": " n-callers " callers, " n-callees " callees (side panel)"))

(defn center-of
  "The qn the lens opens on: :carto/qn in CTX (a recenter), else the first
   argument after the lens name in the /hive argv."
  [ctx]
  (or (:carto/qn ctx)
      (first (:args (domain/parse-command ctx)))))

(defn open-file-op
  "The ui/open-file op for a row PAYLOAD, or nil when it names no file."
  [payload]
  (let [file (or (:file payload) (get payload "file"))
        line (or (:line payload) (get payload "line"))]
    (when (string? file)
      (cond-> {:op :ui/open-file :path file}
        (number? line) (assoc :line line)))))

;; =============================================================================
;; Boundary: the pull through PORTS
;; =============================================================================

(declare carto-lens)

(defn- pull
  [ports config qn dir req-fn]
  (let [[server tool args] (req-fn config qn default-depth dir)
        answer ((:mcp-call ports) server tool args)]
    (if-let [err (domain/result-error answer)]
      {:error err}
      {:data ((:json-parse ports) (domain/answer-body (domain/result-text answer)))})))

(defn open-lens
  "The :lens/open of the carto lens: pulls callers and callees of the centre
   qn through PORTS and answers {:fx [show-panel] :text summary}."
  [ports config ctx]
  (let [qn  (center-of ctx)
        dir (or (:cwd ctx) (when-let [cwd (:cwd ports)] (cwd)))]
    (if (str/blank? (str qn))
      {:text "usage: /hive carto <qualified-name>"}
      (let [callers (pull ports config qn dir callers-request)
            callees (pull ports config qn dir callees-request)]
        (if-let [err (or (:error callers) (:error callees))]
          {:text (str "/hive carto failed: " err)}
          (let [cs (neighbors (:data callers) :callers)
                ds (neighbors (:data callees) :callees)]
            {:fx   [(registry/panel-op carto-lens (doc qn (neighborhood qn cs ds) (count cs) (count ds)))]
             :text (summary qn (count cs) (count ds))}))))))

(defn- verb-config
  "The config a verb runs under: the :config port (a zero-arg fn the addon
   hooks supply with the resolved addon config), else the defaults. Verbs
   receive no config argument, and recentering on the default server would
   ignore a configured :hive/server."
  [ports]
  (if-let [config (:config ports)]
    (config)
    (domain/resolve-config nil)))

(def carto-lens
  "The carto neighborhood lens (L1)."
  {:lens/id      :carto
   :lens/title   "Carto"
   :lens/panel   "carto"
   :lens/cursor? true
   :lens/keys    {"enter" {"invoke" "recenter"}
                  "o"     {"invoke" "open"}}
   :lens/open    (fn [ports config ctx] (open-lens ports config ctx))
   :lens/verbs
   {"recenter" (fn [ports row _payload]
                 (when (string? row)
                   (let [out (open-lens ports (verb-config ports) {:carto/qn row})]
                     (run! #((:panel! ports) %) (:fx out))
                     out)))
    "open"     (fn [ports _row payload]
                 (when-let [op (open-file-op payload)]
                   ((:panel! ports) op)))}})
