(ns hive-dirge.host.domain
  "Pure values of the dirge vessel host: identity, the discovery document,
   the vessel target shape and the reply wire. No IO, no state.

   Reply wire (what dirge POSTs to <url>/reply, one JSON object per request):

     {\"action\": \"focus\",    \"target\": \"<agent-id>\"}   focus one agent
     {\"action\": \"unfocus\"}                               back to the grid
     {\"action\": \"next-tab\"}
     {\"action\": \"prev-tab\"}
     {\"action\": \"refresh\"}

   Anything else parses to an {:reply/error ..} value; it is recorded and
   answered 204 like the rest, never thrown back at the client."
  (:require [clojure.string :as str]
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

(defn discovery-doc
  "The discovery document (string keys, JSON-ready). It carries the token, so
   it is only ever written to a 0600 file and never logged."
  [{:keys [port token pid]}]
  (cond-> {"vessel" (name vessel-id)
           "dialect" (name dialect)
           "url" (base-url port)
           "port" port
           "token" token}
    pid (assoc "pid" pid)))

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
  "Wire action -> command keyword."
  {"focus" :olympus/focus
   "unfocus" :olympus/unfocus
   "next-tab" :olympus/next-tab
   "prev-tab" :olympus/prev-tab
   "refresh" :olympus/refresh})

(defn message->command
  "A parsed reply MESSAGE (string-keyed map) as a command value:
   {:command kw} plus :agent-id for :olympus/focus, or {:reply/error reason}."
  [message]
  (if-not (map? message)
    {:reply/error :reply/not-an-object}
    (let [action (get message "action")
          command (get actions action)
          target (get message "target")]
      (cond
        (nil? command) {:reply/error :reply/unknown-action :reply/action action}
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
