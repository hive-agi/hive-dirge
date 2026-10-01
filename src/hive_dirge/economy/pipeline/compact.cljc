(ns hive-dirge.economy.pipeline.compact
  "compact: Collect span -> Promote entries, Anchor, prior Digest ->
   Boundary resolve Handles (IObservationIndex, else put-observation!) ->
   Pipeline IDigestor -> render + validate -> {:summary md} | nil.
   before-compact: observe only, counts into stats.

   Fail-open everywhere: nil answers mean \"host default\" (dirge's built-in
   summarizer runs)."
  (:require [hive-dirge.economy.digest :as dg]
            [hive-dirge.economy.domain :as d]
            [hive-dirge.economy.markdown :as md]
            [hive-dirge.economy.ports :as p]))

;; ---------------------------------------------------------------------------
;; Budget

(def default-budget
  {:economy/digest-ratio      0.2
   :economy/digest-min-tokens 400
   :economy/digest-max-tokens 3000})

(def chars-per-token 4)

(defn budget-chars
  "Digest budget in chars: ratio x span tokens, clamped to [min, max] tokens."
  [config tokens]
  (let [c    (merge default-budget (select-keys config (keys default-budget)))
        lo   (:economy/digest-min-tokens c)
        hi   (:economy/digest-max-tokens c)
        want (if (number? tokens) (int (min hi (* (:economy/digest-ratio c) tokens))) hi)]
    (* chars-per-token (max lo want))))

;; ---------------------------------------------------------------------------
;; Collect / Promote

(defn collect
  [ctx]
  (select-keys ctx [:span :tokens :reason :focus :ctx-max :pressure :session-id]))

(defn promote
  [collected]
  (let [entries (dg/normalize-span (:span collected))]
    (assoc collected :entries entries :has-prior? (boolean (some :digest entries)))))

;; ---------------------------------------------------------------------------
;; Boundary: handles

(defn- count!
  [stats k]
  (swap! stats update k (fnil inc 0)))

(defn- quiet
  "(f) or nil when it throws; a log without the port answers nil."
  [f]
  (try (f) (catch #?(:cljs :default :default Throwable) _ nil)))

(defn- handle-for!
  "The Handle already held for this result (exact :tool-use-id join, then
   tool+body), else a fresh one; nil on failure."
  [{:keys [log stats]} {:keys [tool text handle tool-use-id]}]
  (or handle
      (when tool-use-id (quiet #(p/handle-by-id log tool-use-id)))
      (quiet #(p/handle-of log tool text))
      (try (p/put-observation! log (d/observation {:tool tool :args nil :result text
                                                   :tool-use-id tool-use-id}))
           (catch #?(:cljs :default :default Throwable) _
             (count! stats :digest-handle-errors)
             nil))))

(defn- args-for
  [log {:keys [args]} h]
  (or args (when h (quiet #(p/args-of log h)))))

(defn resolve-handles!
  [{:keys [log] :as env} entries]
  (mapv (fn [e]
          (if (and (= "tool" (:role e)) (not (:digest e)))
            (let [h    (handle-for! env e)
                  args (args-for log e h)]
              (cond-> e
                h    (assoc :handle h)
                args (assoc :args args)))
            e))
        entries))

;; ---------------------------------------------------------------------------
;; Session carry-forward (when the host leaves the prior Digest out of the span)

(defn- session-key
  [session-id]
  (or session-id :no-session))

(defn prior-of
  [{:keys [digests]} session-id]
  (when digests (get @digests (session-key session-id))))

(defn remember!
  [{:keys [digests]} session-id digest]
  (when digests (swap! digests assoc (session-key session-id) digest)))

;; ---------------------------------------------------------------------------
;; Hooks

(defn compact
  "Hook body. `env` is {:log :stats :digestor :digests atom :config :now fn?}.
   A prior Digest in the span wins; the per-session one fills in only when
   the span carries none."
  [{:keys [stats digestor config now] :as env} ctx]
  (try
    (count! stats :compactions)
    (let [{:keys [entries has-prior? tokens session-id]} (promote (collect ctx))
          last-d  (when-not has-prior? (prior-of env session-id))
          _       (cond has-prior? (count! stats :prior-from-span)
                        last-d     (count! stats :prior-from-session))
          entries (cond->> (resolve-handles! env entries)
                    last-d (into [{:i -1 :role "assistant" :text "" :digest last-d}]))
          anchor  (assoc (dg/anchor-of entries) :at (when now (now)))
          budget  (budget-chars config tokens)
          digest  (when digestor
                    (p/digest digestor entries anchor (vec (:citations last-d)) budget))
          text    (when digest (md/render digest))]
      (if (and text (<= (count text) budget) (md/valid-summary? text))
        (do (remember! env session-id digest)
            (count! stats :digests)
            (swap! stats assoc :last-digest {:epoch     (:epoch digest)
                                             :chars     (count text)
                                             :citations (count (:citations digest))})
            {:summary text})
        (do (count! stats :digest-declined)
            nil)))
    (catch #?(:cljs :default :default Throwable) _
      (count! stats :digest-errors)
      nil)))

(defn before-compact
  "Observe only: records the fold the host is about to run."
  [{:keys [stats]} ctx]
  (try
    (let [{:keys [pressure]} ctx]
      (swap! stats
             (fn [s]
               (cond-> (-> s
                           (update :before-compact (fnil inc 0))
                           (assoc :last-before-compact
                                  (select-keys ctx [:count :tokens :pressure :reason :ctx-max])))
                 (number? pressure)
                 (update :max-pressure (fn [m] (if (and (number? m) (> m pressure)) m pressure)))))))
    (catch #?(:cljs :default :default Throwable) _ nil))
  nil)
