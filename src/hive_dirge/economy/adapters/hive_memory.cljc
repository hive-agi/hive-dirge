(ns hive-dirge.economy.adapters.hive-memory
  "ICrystallizer over hive's MCP memory tool, reached through the harness
   ports {:mcp-call :json-parse}. Fail-open: a failed call files nothing and
   seeds nothing, it never throws into a hook."
  (:require [clojure.string :as str]
            [hive-dirge.economy.crystal :as crystal]
            [hive-dirge.economy.ports.crystallizer :as p]))

(def default-server "hive")

(defn add-request
  "mcp-call triple filing `entry` (see hive-dirge.economy.crystal/entry)."
  [server {:keys [type content tags duration directory]}]
  [server "memory"
   (cond-> {"command" "add" "type" type "content" content
            "tags" tags "duration" duration}
     directory (assoc "directory" directory))])

(defn query-request
  "mcp-call triple listing the newest `n` Digest entries of `directory`."
  [server directory n]
  [server "memory"
   (cond-> {"command" "query" "tags" [crystal/tag] "limit" n "verbosity" "full"}
     (and (string? directory) (not (str/blank? directory))) (assoc "directory" directory))])

(defn- answer-text
  [answer]
  (when (and (map? answer) (not (:error answer)) (not (:isError answer)))
    (str/join "\n" (keep :text (:content answer)))))

(defrecord HiveMemoryCrystallizer [server mcp-call json-parse]
  p/ICrystallizer
  (crystallize! [_ entry]
    (try
      (let [[s t args] (add-request server entry)]
        (some? (answer-text (mcp-call s t args))))
      (catch #?(:cljs :default :default Throwable) _ false)))
  (seeds [_ directory n]
    (try
      (let [[s t args] (query-request server directory n)
            text       (answer-text (mcp-call s t args))]
        (if (and text json-parse) (crystal/texts (json-parse text)) []))
      (catch #?(:cljs :default :default Throwable) _ []))))

(defn make-crystallizer
  "Built from {:server :mcp-call :json-parse}; nil without an :mcp-call port."
  [{:keys [server mcp-call json-parse]}]
  (when mcp-call
    (->HiveMemoryCrystallizer (or server default-server) mcp-call json-parse)))
