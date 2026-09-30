(ns hive-dirge.live
  "Live addon instances, their ports resolved at call time, and the tools and
   hooks added from the REPL. Held in defonce atoms, so an /addons reload or
   a re-evaluation keeps them.")

(defonce !addons (atom {}))

(defonce !extras (atom {}))

(defn register!
  "Records `addon` as the live instance for `id`; answers `addon`."
  [id addon]
  (swap! !addons assoc id addon)
  addon)

(defn addons
  []
  @!addons)

(defn addon
  [id]
  (get @!addons id))

(defn ports
  "`p` itself, or `(p)` when it is a zero-arg fn answering the ports map."
  [p]
  (if (fn? p) (p) p))

(defn extra-tools
  [id]
  (vec (get-in @!extras [id :tools])))

(defn extra-hooks
  [id]
  (get-in @!extras [id :hooks] {}))

(defn tools-with
  "`base` tools with `id`'s extra tools added; an extra replaces the base
   tool of the same :name."
  [id base]
  (let [extra (extra-tools id)
        names (set (map :name extra))]
    (into (vec (remove #(contains? names (:name %)) base)) extra)))

(defn hooks-with
  "`base` hooks with `id`'s extra hooks merged over them."
  [id base]
  (merge base (extra-hooks id)))

(defn add-tool!
  [id tool]
  (swap! !extras update-in [id :tools]
         (fn [ts] (conj (vec (remove #(= (:name %) (:name tool)) ts)) tool)))
  nil)

(defn remove-tool!
  [id tool-name]
  (swap! !extras update-in [id :tools]
         (fn [ts] (vec (remove #(= (:name %) tool-name) ts))))
  nil)

(defn add-hook!
  [id k f]
  (swap! !extras assoc-in [id :hooks k] f)
  nil)

(defn remove-hook!
  [id k]
  (swap! !extras update-in [id :hooks] dissoc k)
  nil)

(defn clear-extras!
  ([] (reset! !extras {}) nil)
  ([id] (swap! !extras dissoc id) nil))
