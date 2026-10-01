(ns hive-dirge.economy.pipeline.watch
  ":dirge/event observers. A :tool-call and its :tool-result (paired by :id)
   become one Observation; turn, usage, run and compaction events count into
   stats. Watch only: answers nil."
  (:require [clojure.string :as str]
            [hive-dirge.economy.pipeline.observe :as observe]))

(def max-pending 256)

(def usage-keys
  [:input-tokens :cached-input-tokens :cache-creation-input-tokens :output-tokens])

(defn event-kind
  "The event's :event as a keyword (cljrs sends a keyword, JSON a string)."
  [ctx]
  (let [e (:event ctx)]
    (cond (keyword? e) e
          (string? e)  (keyword e)
          :else        nil)))

(defn clipped?
  "True when dirge cut `text` at its 16 KiB event limit."
  [text]
  (boolean (and (string? text)
                (str/ends-with? text " more bytes]")
                (str/includes? text "…["))))

(defn- bump!
  [stats k]
  (swap! stats update k (fnil inc 0)))

(defn on-tool-call
  [{:keys [pending]} ctx]
  (when pending
    (swap! pending
           (fn [m]
             (assoc (if (>= (count m) max-pending) {} m)
                    (str (:id ctx)) (select-keys ctx [:tool :args]))))))

(defn on-tool-result
  [{:keys [pending stats] :as env} ctx]
  (let [id   (str (:id ctx))
        call (when pending (get @pending id))
        out  (:output ctx)]
    (when pending (swap! pending dissoc id))
    (when-not call (bump! stats :observe-unpaired))
    (when (clipped? out) (bump! stats :observe-clipped))
    (observe/boundary! env (observe/promote {:tool   (:tool call)
                                             :args   (:args call)
                                             :result out}))))

(defn on-turn-start
  [{:keys [stats]} ctx]
  (swap! stats #(-> % (update :turns (fnil inc 0)) (assoc :last-turn (:index ctx)))))

(defn on-usage
  [{:keys [stats]} ctx]
  (let [seen (into {} (filter (comp number? val)) (select-keys ctx usage-keys))]
    (swap! stats update :usage #(merge-with + (or % {}) seen))))

(defn on-done
  [{:keys [stats]} ctx]
  (swap! stats #(-> % (update :runs (fnil inc 0)) (assoc :last-run-tokens (:tokens ctx)))))

(defn on-compaction-started
  [{:keys [stats]} ctx]
  (swap! stats #(-> %
                    (update :compaction-started (fnil inc 0))
                    (assoc :last-compaction-started (select-keys ctx [:tokens-before])))))

(defn on-context-compacted
  [{:keys [stats]} ctx]
  (swap! stats #(-> %
                    (update :compacted (fnil inc 0))
                    (assoc :last-compacted (select-keys ctx [:tokens-before :tokens-after :kind])))))

(defn on-event
  "Hook body. `env` is {:log :stats :pending atom}."
  [{:keys [stats] :as env} ctx]
  (try
    (bump! stats :events)
    (case (event-kind ctx)
      :tool-call          (on-tool-call env ctx)
      :tool-result        (on-tool-result env ctx)
      :turn-start         (on-turn-start env ctx)
      :usage              (on-usage env ctx)
      :done               (on-done env ctx)
      :compaction-started (on-compaction-started env ctx)
      :context-compacted  (on-context-compacted env ctx)
      nil)
    (catch #?(:cljs :default :default Throwable) _
      (bump! stats :watch-errors)))
  nil)
