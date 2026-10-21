(ns hive-dirge.economy.crystal
  "Crystals: an epoch Digest filed as a hive memory entry, and the seed text a
   new session reads back from the last few of them. Pure; the effects live
   behind hive-dirge.economy.ports/ICrystallizer."
  (:require [clojure.string :as str]))

(def tag "economy-digest")

(def default-seed-limit 3)

(def default-seed-chars 4000)

(defn- present?
  [s]
  (and (string? s) (not (str/blank? s))))

(defn entry
  "Memory entry for the Digest rendered as `md` in `session-id` under `cwd`:
   {:type :content :tags :duration :directory}. nil when `md` is blank."
  [md {:keys [session-id cwd epoch]}]
  (when (present? md)
    (cond-> {:type     "note"
             :content  md
             :tags     (cond-> [tag]
                         (present? session-id) (conj (str "session:" session-id))
                         (some? epoch)         (conj (str "epoch:" epoch)))
             :duration "medium"}
      (present? cwd) (assoc :directory cwd))))

(defn- item-text
  [item]
  (cond
    (string? item) item
    (map? item)    (some #(when (present? %) %) [(:content item) (:preview item)])
    :else          nil))

(defn texts
  "Digest texts out of a parsed memory query answer: a bare vector, or a map
   under :results, :entries or :data. Anything else answers []."
  [data]
  (let [items (cond
                (sequential? data) data
                (map? data)        (some #(let [v (get data %)] (when (sequential? v) v))
                                         [:results :entries :data])
                :else              nil)]
    (into [] (comp (keep item-text) (filter present?)) items)))

(defn seed-context
  "Session-start context from the newest-first `texts`: at most `limit` of
   them, joined under a header and clipped to `max-chars`; nil when none."
  ([texts] (seed-context texts default-seed-limit default-seed-chars))
  ([texts limit max-chars]
   (let [picked (take limit (filter present? texts))]
     (when (seq picked)
       (let [s (str "Digests of earlier sessions in this project (newest first):\n\n"
                    (str/join "\n\n---\n\n" picked))]
         (if (> (count s) max-chars) (subs s 0 max-chars) s))))))
