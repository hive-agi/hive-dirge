(ns hive-dirge.economy.pipeline.observe
  "after-tool-call: Collect {tool args result} -> Promote Observation ->
   Boundary put-observation!. Answers nil: dirge appends nothing."
  (:require [hive-dirge.economy.domain :as d]
            [hive-dirge.economy.ports :as p]))

(def skipped-tools
  "Tools whose results are never logged (retrievals would re-log the log)."
  #{"context_retrieve"})

(defn collect
  [ctx]
  (select-keys ctx [:tool :args :result :error?]))

(defn promote
  [collected]
  (when-not (contains? skipped-tools (str (:tool collected)))
    (d/observation collected)))

(defn boundary!
  "Fail-open: a throwing log counts :observe-errors."
  [{:keys [log stats]} obs]
  (when obs
    (try
      (let [h (p/put-observation! log obs)]
        (swap! stats update :observed (fnil inc 0))
        h)
      (catch #?(:cljs :default :default Throwable) _
        (swap! stats update :observe-errors (fnil inc 0))
        nil))))

(defn after-tool-call
  "Hook body. `env` is {:log IObservationLog :stats atom}."
  [env ctx]
  (boundary! env (promote (collect ctx)))
  nil)
