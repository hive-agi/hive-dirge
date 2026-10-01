(ns hive-dirge.host.domain
  "Pure values of the dirge vessel host: identity, the discovery document,
   the vessel target shape and the reply wire. No IO, no state.

   Reply wire (what dirge POSTs to <url>/reply, one JSON object per request):

     {\"action\": \"focus\",    \"target\": \"<agent-id>\"}   focus one agent
     {\"action\": \"unfocus\"}                               back to the grid
     {\"action\": \"next-tab\"}
     {\"action\": \"prev-tab\"}
     {\"action\": \"refresh\"}
     {\"action\": \"invoke\", \"panel\": \"<panel id>\", \"verb\": \"<verb>\",
      \"row\": \"<row id or null>\", \"payload\": {...}}        a lens verb

   The five olympus actions route to the IOlympusControl port, unchanged.
   An invoke routes to the InvokeRouter port when one is installed (it holds
   the lens registry); an unknown panel or verb is the router's own
   ignore-and-warn decision, never a wire error, so dirge keeps its defaults
   as C3 specifies.

   A well-formed reply is answered 202 as soon as it is queued; the action
   runs afterwards and its re-render reaches dirge on the SSE stream.
   Anything else parses to an {:reply/error ..} value, is recorded and is
   answered 400 at once."
  (:require [clojure.string :as str]
            [hive-dirge.sense.domain :as sense]
            [hive-dirge.lens.registry :as lens]
            [hive-vessel.wire :as wire]))

;; SPDX-License-Identifier: MIT

(def addon-id "hive.dirge.host")

(def vessel-id :dirge)

(def dialect :json)

(def discovery-file-name "dirge.json")

(def route-prefix
  "Path prefix of hive-vessel's SSE bridge routes; the discovery url ends in
   it, so a client reaches <url>/events, <url>/reply and <url>/health."
  "/vessel")

(defn discovery-path
  "The discovery file under XDG-RUNTIME-DIR (nil falls back to /tmp)."
  [xdg-runtime-dir]
  (str (if (str/blank? xdg-runtime-dir) "/tmp" xdg-runtime-dir)
       "/hive-vessel/" discovery-file-name))

(defn base-url
  "Loopback url of the bridge on PORT, up to and including the route prefix."
  [port]
  (str "http://127.0.0.1:" port route-prefix))

(def feature-set-version
  "Version 1 of the subscription feature set: a dirge client subscribes with
   GET /vessel/events?token=..&features=spans,keys,cursor,open-file and the
   bridge records the parsed set per client, readable through
   boundary/client-features and exposed as :vessel/features on the target.
   Bump only for a breaking change to the `features` param semantics;
   additive features never move it. The mirror of
   hive-vessel.executor.handshake/feature-set-version in the worktree
   branch lens-c3-features; deps.edn still pins hive-vessel 0.1.12, which
   lacks it, so this repo owns its copy until the pin moves. See
   docs/lenses.md."
  1)

(defn discovery-doc
  "The discovery document (string keys, JSON-ready). It carries the token, so
   it is only ever written to a 0600 file and never logged."
  [{:keys [port token pid lenses]}]
  (cond-> {"vessel" (name vessel-id)
           "dialect" (name dialect)
           "url" (base-url port)
           "port" port
           "token" token}
    pid (assoc "pid" pid)
    lenses (assoc "capabilities" (merge {"version" 1
                                          "replies" ["focus" "unfocus" "next-tab" "prev-tab" "refresh" "invoke" "ack"]}
                                          (lens/capabilities-fragment lenses)))))

(defn discovery-json [info] (wire/write-json (discovery-doc info)))

(defn target
  "The hive-vessel target for :dirge, executing :json natives with EXECUTE!."
  [execute!]
  {:vessel/id vessel-id
   :vessel/dialect dialect
   :vessel/features #{}
   :vessel/execute! execute!})

;; =============================================================================
;; Reply wire -> command
;; =============================================================================

(def actions
  "Wire action -> command keyword. The five olympus actions are the closed
   set routed to the IOlympusControl port."
  {"focus" :olympus/focus
   "unfocus" :olympus/unfocus
   "next-tab" :olympus/next-tab
   "prev-tab" :olympus/prev-tab
   "refresh" :olympus/refresh})

(def invoke-action
  "The wire action carrying a lens verb invocation."
  "invoke")

(defn message->command
  "A parsed reply MESSAGE (string-keyed map) as a command value:
   {:command kw} plus :agent-id for :olympus/focus, :invoke fields for the
   invoke action, or {:reply/error reason}."
  [message]
  (if-not (map? message)
    {:reply/error :reply/not-an-object}
    (let [action (get message "action")
          command (get actions action)
          target (get message "target")]
      (cond
        (= sense/ack-action action)
        (sense/ack-command message)

        (nil? command)
        (if (= invoke-action action)
          (let [panel (get message "panel")
                verb  (get message "verb")]
            (if (and (string? panel) (not (str/blank? panel))
                     (string? verb) (not (str/blank? verb)))
              {:command :invoke
               :invoke  {"panel"   panel
                         "verb"    verb
                         "row"     (get message "row")
                         "payload" (or (get message "payload") {})}}
              {:reply/error :reply/malformed-invoke :reply/action action}))
          {:reply/error :reply/unknown-action :reply/action action})
        (= :olympus/focus command)
        (if (and (string? target) (not (str/blank? target)))
          {:command command :agent-id target}
          {:reply/error :reply/missing-target :reply/action action})
        :else {:command command}))))

(defn parse-reply
  "RAW (the POST body) as a command value; unparseable JSON is an error value."
  [raw]
  (let [message (try (wire/read-json raw) (catch Exception _ ::unparseable))]
    (if (= ::unparseable message)
      {:reply/error :reply/unparseable}
      (message->command message))))

;; =============================================================================
;; Reply answer
;; =============================================================================

(def max-reply-bytes
  "Largest POST body read; a bigger one is answered 413."
  65536)

(def reply-queue-capacity
  "Commands that may wait for the worker; one more is answered 503."
  64)

(defn reply-admission
  "HTTP status refusing a /reply request before its body is read, or nil.
   Same order as hive-vessel's bridge: Origin, then token, then method."
  [{:keys [origin-allowed? token-ok? method]}]
  (cond
    (not origin-allowed?) 403
    (not token-ok?) 401
    (not= "POST" method) 405
    :else nil))

(defn reply-outcome
  "What happened to COMMAND (a parse-reply value) when offered to the queue;
   ACCEPTED? is the queue's answer (ignored for an error value)."
  [command accepted?]
  (cond
    (:reply/error command) command
    accepted? {:reply/accepted (:command command)}
    :else {:reply/error :reply/queue-full :command (:command command)}))

(defn reply-status
  "HTTP status answering OUTCOME (a reply-outcome value)."
  [outcome]
  (case (:reply/error outcome)
    nil 202
    :reply/queue-full 503
    400))
