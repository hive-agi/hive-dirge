(ns hive-dirge.economy.pipeline.retrieve
  "context_retrieve: Collect params -> Promote handle + range -> Boundary
   fetch. Every call is counted in stats (:retrieves :hits :misses)."
  (:require [hive-dirge.economy.domain :as d]
            [hive-dirge.economy.ports :as p]))

(defn collect
  [params]
  (select-keys params [:handle :start :end :unit]))

(defn promote
  [{:keys [handle] :as collected}]
  {:raw    handle
   :handle (d/parse-handle handle)
   :range  (d/parse-range collected)})

(defn unknown-answer
  [raw]
  (str "unknown handle " (if (string? raw) raw (pr-str raw))
       ": nothing was recorded under it in this session"))

(defn- fetch!
  "Fail-open: a throwing log counts :fetch-errors and reads as a miss."
  [log stats handle rng]
  (try
    (p/fetch log handle rng)
    (catch #?(:cljs :default :default Throwable) _
      (swap! stats update :fetch-errors (fnil inc 0))
      nil)))

(defn boundary!
  "Answers {:text .. :found? bool}."
  [{:keys [log stats]} {:keys [raw handle range]}]
  (swap! stats update :retrieves (fnil inc 0))
  (if-let [text (when handle (fetch! log stats handle range))]
    (do (swap! stats update :hits (fnil inc 0))
        {:text text :found? true})
    (do (swap! stats update :misses (fnil inc 0))
        {:text (unknown-answer raw) :found? false})))

(defn retrieve
  [env params]
  (boundary! env (promote (collect params))))

(defn tool-answer
  [{:keys [text found?]}]
  {:content [{:type "text" :text text}] :isError (not found?)})
