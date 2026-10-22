(ns hive-dirge.economy.pipeline.shape
  "transform-context: Collect messages -> IContextShaper/wanted (pure) ->
   Boundary resolve each wanted result's Handle on the IObservationLog ->
   IContextShaper/shape (pure) -> {:messages shaped} | nil.

   Fail-open everywhere: nil means the model call sees the messages as they
   are. The saved conversation is never touched."
  (:require [hive-dirge.economy.domain :as d]
            [hive-dirge.economy.ports :as p]))

(defn- count!
  [stats k]
  (swap! stats update k (fnil inc 0)))

(defn- quiet
  [f]
  (try (f) (catch #?(:cljs :default :default Throwable) _ nil)))

(defn handle-of!
  "The Handle that retrieves `w`'s text: the exact tool-use-id join, then
   tool+body, else logged now. nil when the log cannot answer, so the result
   stays whole."
  [log {:keys [tool-use-id tool args text]}]
  (or (when tool-use-id (quiet #(p/handle-by-id log tool-use-id)))
      (quiet #(p/handle-of log tool text))
      (quiet #(p/put-observation! log (d/observation {:tool tool :args args :result text
                                                      :tool-use-id tool-use-id})))))

(defn resolve-handles!
  "{index handle} for the `wanted` entries whose Handle the log yields."
  [log wanted]
  (into {} (keep (fn [w] (when-let [h (handle-of! log w)] [(:index w) h]))) wanted))

(defn transform-context
  "Hook body. `env` is {:log :stats :shaper IContextShaper-or-nil}."
  [{:keys [log stats shaper]} ctx]
  (try
    (let [messages (:messages ctx)]
      (when (and shaper (sequential? messages) (seq messages))
        (let [ws (p/wanted shaper messages)]
          (when (seq ws)
            (let [handles (resolve-handles! log ws)
                  shaped  (p/shape shaper messages handles)]
              (when (seq handles)
                (count! stats :shaped)
                (swap! stats update :masked (fnil + 0) (count handles))
                {:messages shaped}))))))
    (catch #?(:cljs :default :default Throwable) _
      (count! stats :shape-errors)
      nil)))
