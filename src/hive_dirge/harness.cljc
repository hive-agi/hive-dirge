(ns hive-dirge.harness
  "dirge.harness from portable addon code: calls through when running inside
   dirge's cljrs host, returns nil anywhere else.")

(defn- harness-fn
  [fn-name]
  (resolve (symbol "dirge.harness" fn-name)))

(defn in-dirge?
  []
  (some? (harness-fn "notify")))

(defn notify!
  "A line in dirge's chat area. `level` is :info (default), :warn or :error."
  ([msg] (notify! msg :info))
  ([msg level]
   (when-let [f (harness-fn "notify")]
     (f msg level))
   nil))

(defn log!
  "A tracing event on dirge's dirge::addon target."
  [level msg]
  (when-let [f (harness-fn "log")]
    (f level msg))
  nil)
