(ns hive-dirge.economy.registry
  "Strategy registry: {role {name (fn [config] impl)}}. A new strategy is a
   map entry, never a case edit. The addon config picks one per role, e.g.
   {:economy/digestor :structured}; an unknown name falls back to the
   role's default."
  (:require [hive-dirge.economy.adapters.structured :as structured]))

(def strategies
  {:digestor {:structured structured/make-digestor}})

(def defaults
  {:digestor :structured})

(def config-keys
  {:digestor :economy/digestor})

(defn- ->kw
  [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword x)
    :else        nil))

(defn strategy-name
  "The registered strategy name `config` selects for `role`."
  [strategies role config]
  (let [wanted (->kw (get config (get config-keys role)))]
    (if (contains? (get strategies role) wanted)
      wanted
      (get defaults role))))

(defn select
  "An impl for `role` built from `config`, or nil when none is registered."
  ([role config] (select strategies role config))
  ([strategies role config]
   (when-let [ctor (get-in strategies [role (strategy-name strategies role config)])]
     (ctor config))))
