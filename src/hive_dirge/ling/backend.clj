(ns hive-dirge.ling.backend
  "DirgeHeadlessBackend: hive-spi IHeadlessBackend over IDirgeSession, one
   dirge session per ling id. Sessions come from an injected factory
   (fn [session-opts] IDirgeSession), by default
   hive-dirge.ling.process/dirge-session.

   spawn! opens the session and sends the task when one is given; dispatch!
   sends one prompt and returns at once; status reads the folded session
   summary; kill! closes; interrupt! cancels the running turn. Spawn and
   dispatch failures throw ex-info carrying :error, as the protocol asks."
  (:require [clojure.string :as str]
            [hive-dirge.ling.domain :as d]
            [hive-dirge.ling.ports :as p]
            [hive-dirge.ling.process :as process]
            [hive-dsl.result :as r]
            [hive-spi.addon.headless :as headless]))

;; SPDX-License-Identifier: MIT

(def backend-id :dirge)

(def capabilities #{:cap/streaming :cap/multi-turn :cap/interrupts})

;; =============================================================================
;; Pure
;; =============================================================================

(defn task-text
  "The non-blank :task of M as a string, else nil."
  [m]
  (let [t (:task m)]
    (when (and (some? t) (not (str/blank? (str t))))
      (str t))))

(defn session-opts
  "Session options for spawn CTX and OPTS over the backend DEFAULTS. ON-EVENT,
   when given, is (fn [ling-id event]) and is bound to the ctx id."
  [defaults ctx opts on-event]
  (let [id (:id ctx)
        cwd (or (:cwd ctx) (:cwd defaults))]
    (cond-> (merge defaults (select-keys opts [:mcp-servers :permission]))
      cwd (assoc :cwd cwd)
      (seq (:env-extra opts)) (update :env merge (:env-extra opts))
      on-event (assoc :on-event (fn [event] (on-event id event))))))

(defn closed?
  "True when TRANSCRIPT holds the session's :ling/closed event."
  [transcript]
  (boolean (some #(= :ling/closed (:event %)) transcript)))

(defn slave-status
  "hive slave status of a session: :dead once closed, :working while a turn
   runs, else :idle."
  [closed summary]
  (cond
    closed :dead
    (= :running (:status summary)) :working
    :else :idle))

(defn status-map
  "Status of ling ID from its session SUMMARY and TRANSCRIPT."
  [id summary transcript]
  (let [cost (d/cost-usd summary)
        progress (d/progress-fraction summary)]
    (cond-> {:slave/id id
             :slave/status (slave-status (closed? transcript) summary)
             :ling/spawn-mode backend-id
             :dirge/turns (:turns summary)
             :dirge/stop-reason (:stop-reason summary)
             :dirge/usage (:usage summary)}
      cost (assoc :ling/cost-usd cost)
      progress (assoc :ling/progress progress))))

;; =============================================================================
;; Effects
;; =============================================================================

(defn- refuse! [message data]
  (throw (ex-info message data)))

(defn session
  "The live session of ling ID in BACKEND, or nil."
  [backend id]
  (get @(:sessions backend) id))

(defn- prompt-or-refuse! [s id text]
  (let [res (p/prompt! s text)]
    (if (r/ok? res)
      (:ok res)
      (refuse! "dirge refused the prompt." {:ling-id id :error (:error res) :result res}))))

(defn- claim! [sessions id s]
  (let [[old _] (swap-vals! sessions (fn [m] (if (contains? m id) m (assoc m id s))))]
    (not (contains? old id))))

(defn- spawn! [{:keys [sessions make-session defaults on-event]} ctx opts]
  (let [id (:id ctx)]
    (when (str/blank? (str id))
      (refuse! "Spawn needs a ling id." {:id id :error :ling/no-id}))
    (when (contains? @sessions id)
      (refuse! "Ling already has a dirge session." {:id id :error :ling/already-spawned}))
    (let [s (make-session (session-opts defaults ctx opts on-event))
          opened (p/open! s)]
      (when (r/err? opened)
        (p/close! s)
        (refuse! "dirge session did not open." {:id id :error (:error opened) :result opened}))
      (when-not (claim! sessions id s)
        (p/close! s)
        (refuse! "Ling already has a dirge session." {:id id :error :ling/already-spawned}))
      (when-let [text (task-text opts)]
        (try (prompt-or-refuse! s id text)
             (catch Throwable t
               (swap! sessions dissoc id)
               (p/close! s)
               (throw t))))
      id)))

(defn- dispatch! [backend ctx task-opts]
  (let [id (:id ctx)
        s (or (session backend id)
              (refuse! "No dirge session for ling." {:ling-id id :error :ling/unknown-session}))
        text (or (task-text task-opts)
                 (refuse! "Dispatch needs a task." {:ling-id id :error :ling/no-task}))]
    (prompt-or-refuse! s id text)
    true))

(defn- status [backend ctx ds-status]
  (let [id (:id ctx)]
    (if-let [s (session backend id)]
      (merge ds-status (status-map id (p/events s) (p/transcript s)))
      (some-> ds-status (assoc :dirge/session :unknown)))))

(defn- kill! [{:keys [sessions]} ctx]
  (let [id (:id ctx)
        [old _] (swap-vals! sessions dissoc id)]
    (if-let [s (get old id)]
      (do (p/close! s) {:killed? true :id id})
      {:killed? false :id id :reason :ling/unknown-session})))

(defn- interrupt! [backend ctx]
  (let [id (:id ctx)]
    (if-let [s (session backend id)]
      (let [res (p/cancel! s)]
        (if (r/ok? res)
          {:success? true :ling-id id}
          {:success? false :ling-id id :reason (:error res)}))
      {:success? false :ling-id id :reason :ling/unknown-session})))

(defn close-all!
  "Close every session of BACKEND. -> the ling ids closed."
  [{:keys [sessions]}]
  (let [[old _] (reset-vals! sessions {})]
    (doseq [s (vals old)] (p/close! s))
    (vec (keys old))))

(defrecord DirgeHeadlessBackend [sessions make-session defaults on-event]
  headless/IHeadlessBackend
  (headless-id [_] backend-id)
  (headless-spawn! [this ctx opts] (spawn! this ctx opts))
  (headless-dispatch! [this ctx task-opts] (dispatch! this ctx task-opts))
  (headless-status [this ctx ds-status] (status this ctx ds-status))
  (headless-kill! [this ctx] (kill! this ctx))
  (headless-interrupt! [this ctx] (interrupt! this ctx))

  headless/IHeadlessCapabilities
  (declared-capabilities [_] capabilities))

(defn dirge-backend
  "A DirgeHeadlessBackend. Opts: :make-session (fn [session-opts]
   IDirgeSession), default process/dirge-session; :defaults, session opts
   under every spawn (:bin :args :permission :mcp-servers :timeout-ms ...);
   :on-event (fn [ling-id event]), called on the session's reader thread."
  ([] (dirge-backend {}))
  ([{:keys [make-session defaults on-event]}]
   (->DirgeHeadlessBackend (atom {})
                           (or make-session process/dirge-session)
                           (or defaults {})
                           on-event)))
