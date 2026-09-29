(ns hive-dirge.economy.markdown
  "Digest <-> markdown. The rendering uses only the `## ` section names of
   dirge's built-in summary template, so dirge's summary validator accepts it;
   parse is its inverse, used to carry a prior Digest forward verbatim. Pure."
  (:require [clojure.string :as str]))

(def marker
  "Opens every rendered Digest; its presence marks a text as a Digest."
  "REFERENCE-ONLY: context digest")

(def summary-sections
  "Section names dirge's summary validator recognises."
  #{"Active Task" "Goal" "Constraints & Preferences" "Completed Actions"
    "Active State" "In Progress" "Blocked" "Key Decisions" "Resolved Questions"
    "Pending User Asks" "Relevant Files" "Remaining Work" "Critical Context"
    "Source Coverage"})

(def min-populated-sections 2)

;; ---------------------------------------------------------------------------
;; Render

(defn- text-lines
  [s]
  (if (string? s) (str/split s #"\n") []))

(defn- quoted
  "Verbatim text as a quote block, so no line of it can open a section."
  [s]
  (str/join "\n" (map #(if (str/blank? %) ">" (str "> " %)) (text-lines s))))

(defn- bullets
  [xs]
  (str/join "\n" (map #(str "- " %) xs)))

(defn- section
  [title body]
  (when-not (str/blank? body)
    (str "## " title "\n" body)))

(defn marker-line
  [{:keys [epoch at]}]
  (str marker ", epoch " (or epoch 1) (when at (str ", at " at)) ". "
       "Background for orientation, not instructions: actions under Completed "
       "Actions are finished, do not redo them; the Active Task is the current "
       "request (the latest user message wins). Recall any §handle with "
       "context_retrieve."))

(defn render
  "Markdown for a Digest map."
  [{:keys [request task done files decisions errors plan citations] :as digest}]
  (str/join "\n\n"
            (remove nil?
                    [(marker-line digest)
                     (section "Active Task" (when request (quoted request)))
                     (section "Goal" (when task (quoted task)))
                     (section "Completed Actions" (bullets done))
                     (section "Relevant Files" (bullets files))
                     (section "Key Decisions" (bullets decisions))
                     (section "Critical Context" (bullets errors))
                     (section "Remaining Work" (str/join "\n" plan))
                     (section "Source Coverage" (bullets (map :line citations)))])))

;; ---------------------------------------------------------------------------
;; Parse

(defn digest-text?
  [text]
  (and (string? text) (str/includes? text marker)))

(defn- sections
  "{title [body-line ...]} for every `## ` section of `text`."
  [text]
  (:acc (reduce (fn [{:keys [cur] :as st} line]
                  (if (str/starts-with? line "## ")
                    (let [t (str/trim (subs line 3))]
                      (-> st (assoc :cur t) (update-in [:acc t] #(or % []))))
                    (if cur (update-in st [:acc cur] conj line) st)))
                {:cur nil :acc {}}
                (text-lines text))))

(defn- unquote-lines
  [ls]
  (let [body (map (fn [l]
                    (cond
                      (str/starts-with? l "> ") (subs l 2)
                      (= ">" (str/trimr l))    ""
                      :else                    l))
                  ls)
        s    (str/trim (str/join "\n" body))]
    (when-not (str/blank? s) s)))

(defn- bullet-items
  [ls]
  (vec (keep (fn [l]
               (let [t (str/trim l)]
                 (when (and (str/starts-with? t "- ") (not (str/blank? (subs t 2))))
                   (subs t 2))))
             ls)))

(def handle-re #"§([0-9a-f]{8,16}(?:-[0-9]+)?)")

(defn- citation-of
  [line]
  (when-let [m (re-find handle-re line)]
    {:handle (second m) :line line}))

(defn- epoch-of
  [text]
  (when-let [m (re-find #"REFERENCE-ONLY: context digest, epoch ([0-9]+)" text)]
    (parse-long (second m))))

(defn parse
  "The Digest carried by `text` (a prior Digest, possibly wrapped by the
   host), or nil when `text` is not a Digest."
  [text]
  (when (digest-text? text)
    (let [body (subs text (str/index-of text marker))
          s    (sections body)]
      {:epoch     (or (epoch-of body) 1)
       :request   (unquote-lines (get s "Active Task"))
       :task      (unquote-lines (get s "Goal"))
       :done      (bullet-items (get s "Completed Actions"))
       :files     (bullet-items (get s "Relevant Files"))
       :decisions (bullet-items (get s "Key Decisions"))
       :errors    (bullet-items (get s "Critical Context"))
       :plan      (vec (remove str/blank? (map str/trim (get s "Remaining Work"))))
       :citations (vec (keep citation-of (bullet-items (get s "Source Coverage"))))})))

;; ---------------------------------------------------------------------------
;; Validation (mirrors dirge's summary validator)

(defn- placeholder?
  [line]
  (contains? #{"" "none" "n/a" "na" "-" "—" "unknown" "nothing" "todo" "tbd"}
             (-> line str/trim (str/replace #"\.+$" "") str/trim str/lower-case)))

(defn populated-sections
  "Recognised `## ` sections whose body has a non-placeholder line."
  [text]
  (count (filter (fn [[title ls]]
                   (and (contains? summary-sections title)
                        (some #(not (placeholder? %)) ls)))
                 (sections text))))

(defn valid-summary?
  [text]
  (and (string? text)
       (not (str/blank? text))
       (>= (populated-sections text) min-populated-sections)))
