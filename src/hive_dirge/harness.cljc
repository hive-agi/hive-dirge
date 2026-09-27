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

(defn cwd
  "dirge's working directory, or nil outside dirge."
  []
  (when-let [f (harness-fn "cwd")]
    (f)))

(defn mcp-call
  "Calls `tool` on dirge's MCP connection to `server` with `args`. Answers the
   MCP tool result, {:error msg} when dirge refuses or the call fails, and nil
   outside dirge."
  [server tool args]
  (when-let [f (harness-fn "mcp-call")]
    (f server tool args)))

(defn json-parse
  "JSON text as data with keyword keys, or nil (unparsable, or outside
   dirge)."
  [text]
  (when-let [f (harness-fn "json-parse")]
    (f text)))

(defn panel!
  "Sends a side-panel op to dirge. True when delivered; nil outside dirge."
  [op]
  (when-let [f (harness-fn "panel")]
    (f op)))
