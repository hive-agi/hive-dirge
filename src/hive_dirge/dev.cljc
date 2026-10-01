(ns hive-dirge.dev
  "REPL helpers for dirge's addon nREPL: list and inspect the live hive-dirge
   addons, add or drop a tool or hook on one, and ask dirge to refresh.

     (require '[hive-dirge.dev :as dev])
     (dev/inspect)                          ; every live addon
     (dev/add-hook! \"hive.dirge\" :dirge/on-prompt (fn [_] \"hi\"))
     (dev/refresh!)"
  (:require [hive-addon.protocol :as p]
            [hive-dirge.harness :as h]
            [hive-dirge.live :as live]))

(defn refresh!
  "Asks dirge to re-read every addon's tools and hooks. True inside dirge."
  []
  (boolean (h/refresh!)))

(defn ids
  []
  (vec (sort (keys (live/addons)))))

(defn- attempt
  [f]
  (try (f)
       (catch #?(:cljs :default :default Throwable) t
         {:error (or (ex-message t) (str t))})))

(defn inspect
  "{:id :tools :hooks :health :extras} for one live addon, or a map of them
   all by id."
  ([] (into {} (map (juxt identity inspect)) (ids)))
  ([id]
   (when-let [a (live/addon id)]
     (let [tools (attempt #(p/tools a))
           hooks (attempt #(p/hooks a))]
       {:id     id
        :tools  (if (sequential? tools) (mapv :name tools) tools)
        :hooks  (if (:error hooks) hooks (vec (sort (map str (keys hooks)))))
        :health (attempt #(p/health a))
        :extras {:tools (mapv :name (live/extra-tools id))
                 :hooks (vec (sort (map str (keys (live/extra-hooks id)))))}}))))

(defn add-tool!
  [id tool]
  (live/add-tool! id tool)
  (refresh!))

(defn remove-tool!
  [id tool-name]
  (live/remove-tool! id tool-name)
  (refresh!))

(defn add-hook!
  [id k f]
  (live/add-hook! id k f)
  (refresh!))

(defn remove-hook!
  [id k]
  (live/remove-hook! id k)
  (refresh!))

(defn reset-extras!
  ([] (live/clear-extras!) (refresh!))
  ([id] (live/clear-extras! id) (refresh!)))
