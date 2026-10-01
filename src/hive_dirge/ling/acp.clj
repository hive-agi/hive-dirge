(ns hive-dirge.ling.acp
  "ACP client: AcpDirgeSession implements IDirgeSession over any
   IFrameTransport. Incoming frames are dispatched on the transport's reader
   thread: responses resolve pending requests, session/update notifications
   become ling events (hive-dirge.ling.domain), and agent requests are
   answered (permission by policy, anything else method-not-found)."
  (:require [hive-dirge.ling.domain :as d]
            [hive-dirge.ling.ports :as p]
            [hive-dsl.result :as r]))

;; SPDX-License-Identifier: MIT

(def default-request-timeout-ms 30000)

(def initial-state
  {:next-id 0
   :pending {}
   :session-id nil
   :events []
   :summary d/empty-summary
   :turn nil
   :open? false
   :closed? false})

;; =============================================================================
;; State transitions (pure, applied with swap!)
;; =============================================================================

(defn- record-event [state event]
  (-> state
      (update :events conj event)
      (update :summary d/step event)))

(defn- register-request [state id entry]
  (-> state (assoc-in [:pending id] entry) (update :next-id inc)))

(defn- fail-pending
  "Keeps :turn; the failed prompt's turn-ended! clears it."
  [state]
  (assoc state :pending {} :transport-closed? true :open? false))

;; =============================================================================
;; Effects
;; =============================================================================

(defn- guarded!
  "Call (F & ARGS) for a caller-supplied callback; a throw is kept under
   :callback-errors in STATE instead of reaching the reader thread."
  [state f & args]
  (try (apply f args)
       (catch Throwable callback-failure
         (swap! state update :callback-errors (fnil conj [])
                {:message (ex-message callback-failure)
                 :class (.getName (class callback-failure))})
         nil)))

(defn- emit! [{:keys [state on-event]} event]
  (swap! state record-event event)
  (when on-event (guarded! state on-event event)))

(defn- send! [{:keys [transport]} frame]
  (p/send-frame! transport frame))

(defn- request!
  "Send METHOD built by (BUILD id); -> [id promise]. ON-RESULT, when given,
   runs with the outcome before the promise is delivered."
  ([session build] (request! session build nil))
  ([{:keys [state] :as session} build on-result]
   (let [prom (promise)
         id (:next-id (first (swap-vals! state
                                         (fn [s] (register-request s (:next-id s)
                                                                   {:promise prom
                                                                    :on-result on-result})))))
         sent (send! session (build id))]
     (when (r/err? sent)
       (let [[old _] (swap-vals! state update :pending dissoc id)]
         (when (get-in old [:pending id])
           (when on-result (guarded! state on-result sent))
           (deliver prom sent))))
     [id prom])))

(defn- await! [prom timeout-ms what]
  (let [v (deref prom timeout-ms ::timeout)]
    (if (= ::timeout v)
      (r/err :ling/timeout {:waiting-for what :timeout-ms timeout-ms})
      v)))

;; =============================================================================
;; Incoming frames
;; =============================================================================

(defn- on-response [{:keys [state]} frame]
  (let [id (get frame "id")
        [old _] (swap-vals! state update :pending dissoc id)]
    (when-let [{:keys [promise on-result]} (get-in old [:pending id])]
      (let [outcome (d/response-outcome frame)]
        (when on-result (guarded! state on-result outcome))
        (deliver promise outcome)))))

(defn- on-request [{:keys [permission] :as session} frame]
  (let [id (get frame "id")
        method (get frame "method")]
    (send! session
           (if (= "session/request_permission" method)
             (d/result-response id (d/permission-reply permission (get frame "params")))
             (d/error-response id d/method-not-found (str "client does not handle " method))))))

(defn- on-transport-closed [{:keys [state] :as session} reason]
  (let [[old _] (swap-vals! state fail-pending)
        err (r/err :ling/transport-closed {:reason reason})]
    (doseq [{:keys [promise on-result]} (vals (:pending old))]
      (when on-result (guarded! state on-result err))
      (deliver promise err))
    (when-not (:transport-closed? old)
      (emit! session {:event :ling/closed :session (:session-id old) :reason reason}))))

(defn handle-frame!
  "Dispatch one incoming FRAME for SESSION."
  [session frame]
  (cond
    (and (map? frame) (:transport/closed frame))
    (on-transport-closed session (:transport/closed frame))

    (and (map? frame) (:transport/error frame))
    (emit! session {:event :ling/wire-error :error (:transport/error frame)
                    :line (:line frame)})

    :else
    (case (d/frame-kind frame)
      :response     (on-response session frame)
      :notification (some->> (d/notification->event frame) (emit! session))
      :request      (on-request session frame)
      (emit! session {:event :ling/wire-error :error :acp/invalid-frame :frame frame}))))

;; =============================================================================
;; Session
;; =============================================================================

(defn- turn-ended! [{:keys [state] :as session} turn-promise sid outcome]
  (let [event (if (r/ok? outcome)
                (d/prompt-result->event sid (:ok outcome))
                {:event :ling/turn-end :session sid :stop-reason :error :error outcome})]
    (emit! session event)
    (swap! state (fn [s] (if (identical? turn-promise (get-in s [:turn :promise]))
                           (assoc s :turn nil)
                           s)))
    (deliver turn-promise (if (r/ok? outcome) (r/ok event) outcome))))

(defn- open-session! [{:keys [state transport cwd mcp-servers timeout-ms] :as session}]
  (r/let-ok [_ (p/start! transport #(handle-frame! session %))
             init (await! (second (request! session d/initialize-request))
                          timeout-ms :initialize)
             _ (if (= d/protocol-version (get init "protocolVersion"))
                 (r/ok init)
                 (r/err :acp/protocol-version {:agent (get init "protocolVersion")
                                               :client d/protocol-version}))
             created (await! (second (request! session #(d/new-session-request % cwd mcp-servers)))
                             timeout-ms :session/new)
             sid (if-let [sid (get created "sessionId")]
                   (r/ok sid)
                   (r/err :acp/no-session-id {:result created}))]
    (swap! state assoc :session-id sid :open? true :agent (get init "agentInfo"))
    (r/ok {:session-id sid :agent (get init "agentInfo")})))

(defn- start-turn! [{:keys [state] :as session} text]
  (let [turn-promise (promise)
        [old _] (swap-vals! state (fn [s] (if (or (:turn s) (not (:open? s)) (:closed? s))
                                            s
                                            (-> s
                                                (assoc :turn {:promise turn-promise})
                                                (update :summary d/begin-turn)))))]
    (cond
      (:closed? old) (r/err :ling/closed {})
      (not (:open? old)) (r/err :ling/not-open {})
      (:turn old) (r/err :ling/busy {:session-id (:session-id old)})
      :else
      (let [sid (:session-id old)]
        (request! session #(d/prompt-request % sid text)
                  #(turn-ended! session turn-promise sid %))
        (r/ok {:turn (inc (get-in old [:summary :turns]))})))))

(defn- last-turn-result
  "The last finished turn in EVENTS as a Result: its turn-end event, or the
   error that ended it."
  [events]
  (if-let [end (last (filter #(= :ling/turn-end (:event %)) events))]
    (or (:error end) (r/ok end))
    (r/err :ling/no-turn {})))

(defrecord AcpDirgeSession [state transport cwd mcp-servers permission on-event timeout-ms]
  p/IDirgeSession
  (open! [this]
    (if (:open? @state)
      (r/ok {:session-id (:session-id @state)})
      (let [res (r/rescue (r/err :ling/open-threw {}) (open-session! this))]
        (when (r/err? res) (p/stop! transport))
        res)))
  (prompt! [this text] (start-turn! this text))
  (cancel! [this]
    (if-let [sid (:session-id @state)]
      (r/map-ok (send! this (d/cancel-notification sid)) (constantly true))
      (r/err :ling/not-open {})))
  (collect! [_ timeout-ms]
    (if-let [{:keys [promise]} (:turn @state)]
      (await! promise timeout-ms :turn)
      (last-turn-result (:events @state))))
  (transcript [_] (:events @state))
  (cost [_] (get-in @state [:summary :usage]))
  (events [_] (:summary @state))
  (close! [this]
    (let [[old _] (swap-vals! state assoc :closed? true :open? false)]
      (when-not (:closed? old)
        (p/stop! transport)
        (on-transport-closed this :closed-by-client))
      (r/ok true))))

(defn acp-session
  "An AcpDirgeSession over TRANSPORT. Opts: :cwd (default user.dir),
   :mcp-servers [], :permission :deny|:allow (default :deny), :on-event
   (fn [event]) called on the reader thread, :timeout-ms per request."
  ([transport] (acp-session transport {}))
  ([transport {:keys [cwd mcp-servers permission on-event timeout-ms]}]
   (map->AcpDirgeSession
    {:state (atom initial-state)
     :transport transport
     :cwd (or cwd (System/getProperty "user.dir"))
     :mcp-servers (vec mcp-servers)
     :permission (if (contains? d/permission-policies permission) permission :deny)
     :on-event on-event
     :timeout-ms (or timeout-ms default-request-timeout-ms)})))
