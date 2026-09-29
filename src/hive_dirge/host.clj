(ns hive-dirge.host
  "hive.dirge.host IAddon: the JVM side of the dirge integration.

   initialize! starts hive-vessel's SSE bridge on loopback (random port,
   random token, every browser Origin refused), writes the discovery file
   $XDG_RUNTIME_DIR/hive-vessel/dirge.json {url, token, ...} as 0600, and
   exposes the :dirge vessel target through the hooks hive-olympus.harness
   folds: :vessel/dispatch! and :vessel/target. dirge reads the discovery
   file, subscribes to <url>/events?token=.. and POSTs key presses to
   <url>/reply?token=..; each reply is validated, answered at once (202, or
   4xx for a bad token or body, 503 when the queue is full) and queued; one
   worker routes the queued commands to hive.olympus in arrival order
   (:olympus/focus! :olympus/next-tab! :olympus/prev-tab! :olympus/refresh!),
   and the re-render reaches dirge on the SSE stream.

   Config (manifest :addon/config merged with runtime config):
     :dirge/port             bridge port (default 0 = random)
     :dirge/discovery-path   discovery file (default under $XDG_RUNTIME_DIR)
     :dirge/heartbeat-ms     SSE ': ping' period (default hive-vessel's)
     :dirge/olympus          an IOlympusControl to route replies to (default:
                             hive.olympus's hooks, from :mount/dependencies)
     :dirge/action-queue     (fn [run-command] IActionQueue) holding accepted
                             commands (default: one worker thread over a
                             FIFO of domain/reply-queue-capacity)

   Feature handshake (Lens C3): a dirge client may subscribe with
   `?features=spans,keys,cursor,open-file`; hive-vessel's bridge records the
   parsed set per client and :vessel/target answers it as :vessel/features.
   The dirge show-panel translator gates on it: spans, keys and cursor ride
   only when advertised, else plain lines (row ids always survive). Union
   semantics and the version constant live in docs/lenses.md and
   domain/feature-set-version.

   The token is only ever written to the 0600 discovery file; health and
   metadata never carry it."
  (:require [hive-addon.protocol :as addon]
            [hive-dirge.host.boundary :as boundary]
            [hive-dirge.host.domain :as domain]
            [hive-dirge.host.ports :as ports]
            [hive-vessel.core :as v]
            [hive-vessel.dialect.json :as json]
            [hive-vessel.wire :as wire]))

;; SPDX-License-Identifier: MIT

(def reply-log-size 20)

;; =============================================================================
;; Feature-gated panel feed (Lens C3)
;; =============================================================================

(defn- span-lines-message
  "Resolve the vessel dialect at call time. 0.1.12 has only the one-arity
   implementation; use that plain renderer when the two-arity seam is absent
   (notably when :dev overrides the released pin with a sibling checkout)."
  [op target]
  (let [f (or (resolve 'hive-vessel.dialect.json/show-panel-message)
              #'json/show-panel-message)]
    (if (some #{2} (:arglists (meta f)))
      (f op target)
      (f op))))

(defn- neutral-panel-message [message features]
  ;; The :dev alias may override 0.1.14 with a pre-C5 hive-vessel checkout.
  ;; Those clients never advertise features; retain their legacy vocabulary.
  (if-let [neutralize (resolve 'hive-vessel.dialect.json/neutralize)]
    (neutralize message {:vessel/features features})
    message))

(defn- plain-row [row]
  (let [spans (or (get row "spans") (:spans row))
        text (or (get row "text") (:text row)
                 (apply str (map #(or (get % "text") (:text %)) spans)))]
    (cond-> (dissoc row "spans" :spans)
      spans (assoc "text" text))))

(defn- panel-rows [op spans?]
  (when (seq (:panel/rows op))
    (let [rows (wire/->json-data
                (into [{:text (get-in op [:doc :doc/title]) :face :title}]
                      (:panel/rows op)))]
      (if spans? rows (mapv plain-row rows)))))

(defn- panel-lines [op spans?]
  (let [message (dissoc (span-lines-message op (when spans? {:vessel/features #{:spans}}))
                        "keys" "cursor" "panel/rows" "spans")
        rows (panel-rows op spans?)]
    (cond-> message
      rows (assoc "lines" rows)
      (not spans?) (update "lines" #(mapv plain-row %)))))

(defn panel-message
  "The :json show-panel message for a connected dirge client set whose
   advertised features are FEATURES (the union of the per-client sets the
   bridge records; see boundary/client-features). The client advertised:

     :spans   the \"lines\" carry their span rows:
              {\"face\": .., \"id\": .., \"spans\": [{\"text\": .., \"face\": ..}]};
              lens :panel/rows pass through as rows, ids and payloads included
     :keys    the message carries \"keys\" (dirge chords -> reply verb or
              {\"invoke\" verb}) whenever the op carries :keys
     :cursor  the message carries \"cursor\": true whenever the op carries
              a truthy :cursor

   Anything not advertised degrades: without :spans every span row flattens
   to a plain {text, face, id, payload} line, and the keys/cursor fields are omitted
   -- never a richer message than the client's feature set, never an error.
   There is no top-level \"spans\" key: dirge reads span rows inside \"lines\"
   (docs/panel-feed.md). With no client connected FEATURES is #{}
   (pre-handshake behaviour), so the gates are conservative by construction.
   Vessel's JSON dialect neutralizes names for any v2 feature set: show,
   id, and nested doc title; legacy clients retain qualified names.

   PURE: which features a client set advertises is the only input; the
   bridge read happens in the translator's closure."
  ^java.util.Map
  [features op]
  (let [features (or features #{})
        with-spans (if (contains? features :spans)
                     (panel-lines op true)
                     (panel-lines op false))]
    (neutral-panel-message
     (cond-> with-spans
       (and (contains? features :keys) (:keys op))
       (assoc "keys" (wire/->json-data (:keys op)))
       (and (contains? features :cursor) (:cursor op))
       (assoc "cursor" (wire/->json-data (:cursor op))))
     features)))

(defn- dependency-hooks [config]
  (keep (fn [[_ dep]]
          (when (addon/addon? dep)
            (try (addon/hooks dep) (catch Throwable _ nil))))
        (:mount/dependencies config)))

(defn- register-external-lenses! [config]
  (when-let [hive (get (:mount/dependencies config) "hive.dirge")]
    (when (addon/addon? hive)
      (when-let [register! (:dirge/register-lenses! (addon/hooks hive))]
        (register! (boundary/dependency-lenses
                    (update config :mount/dependencies dissoc "hive.dirge")))))))

(defn- olympus-port [config]
  (or (:dirge/olympus config)
      (boundary/hooks-olympus (fn [] (boundary/olympus-hooks config)))))

(defn- record! [state outcome]
  (swap! state update :replies
         (fn [rs] (vec (take-last reply-log-size (conj (or rs []) (dissoc outcome :result))))))
  outcome)

(defn- run-command-fn
  "What the worker applies to each queued command: route it through OLYMPUS
   and record the outcome."
  [state olympus router]
  (fn [command] (record! state (ports/route! olympus router command))))

(defn- action-queue [config run-command]
  (if-let [make (:dirge/action-queue config)]
    (make run-command)
    (boundary/single-worker-queue {:capacity domain/reply-queue-capacity
                                   :run-command run-command})))

(defn- on-reply-fn
  "Raw POST body -> parsed command -> offered to QUEUE; returns the HTTP
   status at once. Refusals are recorded here, routed outcomes by the worker."
  [state queue]
  (fn [raw]
    (let [command (domain/parse-reply raw)
          outcome (domain/reply-outcome command
                                        (and (not (:reply/error command))
                                             (ports/submit! queue command)))]
      (when (:reply/error outcome) (record! state outcome))
      (domain/reply-status outcome))))

(defn- start! [state seed runtime-config]
  (locking state
    (if (= :active (:lifecycle @state))
      {:success? true :already-initialized? true}
      (let [config (merge seed runtime-config)
            token (boundary/new-token)
            path (or (:dirge/discovery-path config)
                     (domain/discovery-path (System/getenv "XDG_RUNTIME_DIR")))
            lenses (do (register-external-lenses! config)
                       (boundary/dependency-registry config))
            router (boundary/registry-invoke-router
                    {:registry-fn (fn [] (boundary/dependency-registry config))
                     :config config})
            queue (action-queue config (run-command-fn state (olympus-port config) router))
            bridge (boundary/start-bridge! {:port (:dirge/port config)
                                            :token token
                                            :heartbeat-ms (:dirge/heartbeat-ms config)
                                            :on-reply (on-reply-fn state queue)})]
        (try
          (boundary/write-private! path (domain/discovery-json
                                         {:port (:port bridge) :token token
                                          :pid (.pid (java.lang.ProcessHandle/current))
                                          :lenses lenses}))
          (let [features-fn (fn [] (boundary/client-features bridge))
                registry (atom (v/register
                                (v/registry-from-hooks (dependency-hooks config))
                                {:translator/id :hive-dirge/lens-panel
                                 :translator/op :ui/show-panel
                                 :translator/priority 1
                                 :translator/when {:vessel/id :dirge}
                                 :translator/translate
                                 (fn [op _] (json/native (panel-message (features-fn) op)))}))
                target (domain/target (boundary/executor bridge))]
            (swap! state assoc
                   :lifecycle :active :bridge bridge :queue queue :discovery path
                   :registry registry :target target :replies [])
            {:success? true
             :errors []
             :metadata {:port (:port bridge) :discovery path}})
          (catch Throwable t
            (boundary/stop-bridge! bridge)
            (ports/close! queue)
            (reset! state {:lifecycle :failed :last-error (ex-message t)})
            {:success? false :errors [(ex-message t)]}))))))

(defn- stop! [state]
  (locking state
    (let [{:keys [lifecycle bridge queue discovery]} @state]
      (when (= :active lifecycle)
        (boundary/stop-bridge! bridge)
        (ports/close! queue)
        (try (boundary/delete-file! discovery) (catch Throwable _ nil)))
      (reset! state {:lifecycle :stopped})
      nil)))

(defn- health-of [state]
  (let [{:keys [lifecycle bridge discovery replies last-error]} @state]
    {:status (case lifecycle
               :active (if (pos? (:clients (boundary/bridge-status bridge))) :ok :degraded)
               :failed :down
               :degraded)
     :details (cond-> {:lifecycle lifecycle}
                bridge (merge (boundary/bridge-status bridge)
                              {:discovery discovery
                               :replies (count replies)
                               :reply-errors (count (filter :reply/error replies))})
                last-error (assoc :last-error last-error))}))

(defrecord DirgeHostAddon [state seed]
  addon/IAddon
  (addon-id [_] domain/addon-id)
  (addon-type [_] :native)
  (capabilities [_] #{:vessel :health-reporting})
  (initialize! [_ runtime-config] (start! state seed runtime-config))
  (shutdown! [_] (stop! state))
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] (health-of state))
  (excluded-tools [_] #{})
  (hooks [_]
    (let [{:keys [lifecycle bridge registry target]} @state]
      (if (= :active lifecycle)
        {:vessel/target (fn [] (assoc target :vessel/features (boundary/client-features bridge)))
         :vessel/dispatch! (fn [op-or-ops] (v/dispatch! registry target op-or-ops))
         :vessel/register-translators! (fn [translators] (swap! registry v/register-all translators) nil)
         :dirge/bridge (fn [] (boundary/bridge-status bridge))
         :dirge/replies (fn [] (:replies @state))}
        {}))))

(defn addon-ctor
  "Pure constructor resolved from the mount manifest: no port, file or thread."
  [config]
  (->DirgeHostAddon (atom {:lifecycle :created}) (or config {})))
