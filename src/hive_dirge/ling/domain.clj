(ns hive-dirge.ling.domain
  "Pure values of a dirge ling session over ACP: JSON-RPC frames the client
   sends, the classification of frames it receives, the mapping from ACP
   session/update notifications to ling events, and the fold of those events
   into a session summary. No IO, no state.

   Frames are JSON-ready maps with string keys. Events are keyword maps:

     {:event :ling/text        :session sid :text s}
     {:event :ling/thought     :session sid :text s}
     {:event :ling/user-text   :session sid :text s}
     {:event :ling/tool-call   :session sid :tool-id id :title s :kind kw
                               :status kw :input any}
     {:event :ling/tool-update :session sid :tool-id id :status kw
                               :output s :title s :kind kw}
     {:event :ling/plan        :session sid :entries [{:content :priority :status}]
                               :progress {:done n :total n}}
     {:event :ling/commands    :session sid :commands [name]}
     {:event :ling/mode        :session sid :mode s}
     {:event :ling/usage       :session sid :usage Usage}
     {:event :ling/turn-end    :session sid :stop-reason kw :usage Usage}
     {:event :ling/unknown     :session sid :kind s :raw update}"
  (:require [clojure.string :as str]))

;; SPDX-License-Identifier: MIT

(def protocol-version 1)

;; =============================================================================
;; Outgoing frames
;; =============================================================================

(defn request [id method params]
  {"jsonrpc" "2.0" "id" id "method" method "params" params})

(defn notification [method params]
  {"jsonrpc" "2.0" "method" method "params" params})

(defn result-response [id result]
  {"jsonrpc" "2.0" "id" id "result" result})

(defn error-response [id code message]
  {"jsonrpc" "2.0" "id" id "error" {"code" code "message" message}})

(def client-capabilities
  {"fs" {"readTextFile" false "writeTextFile" false} "terminal" false})

(defn initialize-request [id]
  (request id "initialize" {"protocolVersion" protocol-version
                            "clientCapabilities" client-capabilities}))

(defn new-session-request
  "session/new for CWD. MCP-SERVERS is a vector of ACP McpServer maps."
  [id cwd mcp-servers]
  (request id "session/new" {"cwd" (str cwd) "mcpServers" (vec mcp-servers)}))

(defn prompt-request [id session-id text]
  (request id "session/prompt" {"sessionId" session-id
                                "prompt" [{"type" "text" "text" (str text)}]}))

(defn cancel-notification [session-id]
  (notification "session/cancel" {"sessionId" session-id}))

;; =============================================================================
;; Incoming frames
;; =============================================================================

(defn frame-kind
  "-> :response | :notification | :request | :invalid"
  [frame]
  (cond
    (not (map? frame)) :invalid
    (and (contains? frame "method") (contains? frame "id")) :request
    (contains? frame "method") :notification
    (and (contains? frame "id")
         (or (contains? frame "result") (contains? frame "error"))) :response
    :else :invalid))

(defn response-outcome
  "A response frame as {:ok result} or {:error :acp/rpc-error :code :message :data}."
  [frame]
  (if-let [e (get frame "error")]
    {:error :acp/rpc-error
     :code (get e "code") :message (get e "message") :data (get e "data")}
    {:ok (get frame "result")}))

;; =============================================================================
;; Value helpers
;; =============================================================================

(defn- kw
  "\"in_progress\" -> :in-progress; nil and blanks -> nil."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (keyword (str/replace (str/trim s) "_" "-"))))

(defn- items
  "X when it is a JSON array, else empty."
  [x]
  (if (sequential? x) x []))

(defn- block-text
  "Text of one ACP ContentBlock (or ToolCallContent wrapping one), else nil."
  [block]
  (when (map? block)
    (case (get block "type")
      "text"          (get block "text")
      "content"       (block-text (get block "content"))
      "resource_link" (get block "uri")
      "resource"      (get-in block ["resource" "text"])
      "diff"          (get block "path")
      nil)))

(defn- content-text [content]
  (cond
    (sequential? content) (not-empty (str/join (keep block-text content)))
    (map? content) (block-text content)
    :else nil))

(defn- num-at [m ks]
  (some (fn [k] (let [v (get m k)] (when (number? v) v))) ks))

(defn- cost-of [u]
  (let [cost (get u "cost")]
    (or (num-at u ["costUsd" "cost_usd"])
        (when (and (map? cost) (contains? #{nil "USD" "usd"} (get cost "currency")))
          (num-at cost ["amount"]))
        (when (number? cost) cost))))

(defn ->usage
  "Normalise an ACP usage map (camelCase, snake_case or the usage_update
   shape) to {:input-tokens :output-tokens :cached-read-tokens
   :cached-write-tokens :total-tokens :cost-usd :context-used :context-size},
   dropping absent keys. nil when nothing numeric is present."
  [u]
  (when (map? u)
    (not-empty
     (into {} (remove (comp nil? val))
           {:input-tokens        (num-at u ["inputTokens" "input_tokens"])
            :output-tokens       (num-at u ["outputTokens" "output_tokens"])
            :cached-read-tokens  (num-at u ["cachedReadTokens" "cached_read_tokens"
                                             "cacheReadTokens" "cache_read_tokens"])
            :cached-write-tokens (num-at u ["cachedWriteTokens" "cached_write_tokens"
                                             "cacheWriteTokens" "cache_write_tokens"])
            :total-tokens        (num-at u ["totalTokens" "total_tokens"])
            :cost-usd            (cost-of u)
            :context-used        (num-at u ["used"])
            :context-size        (num-at u ["size"])}))))

(defn plan-progress
  "{:done n :total n} over plan ENTRIES (maps with a :status keyword)."
  [entries]
  {:done (count (filter #(= :completed (:status %)) entries))
   :total (count entries)})

;; =============================================================================
;; session/update -> event
;; =============================================================================

(defmulti update->event
  "The ling event for one ACP SessionUpdate map UPD of session SID."
  (fn [_sid upd] (get upd "sessionUpdate")))

(defmethod update->event "agent_message_chunk" [sid upd]
  {:event :ling/text :session sid :text (or (content-text (get upd "content")) "")})

(defmethod update->event "agent_thought_chunk" [sid upd]
  {:event :ling/thought :session sid :text (or (content-text (get upd "content")) "")})

(defmethod update->event "user_message_chunk" [sid upd]
  {:event :ling/user-text :session sid :text (or (content-text (get upd "content")) "")})

(defmethod update->event "tool_call" [sid upd]
  (let [out (content-text (get upd "content"))]
    (cond-> {:event :ling/tool-call :session sid
             :tool-id (get upd "toolCallId")
             :title (get upd "title")
             :kind (or (kw (get upd "kind")) :other)
             :status (or (kw (get upd "status")) :pending)
             :input (get upd "rawInput")}
      out (assoc :output out))))

(defmethod update->event "tool_call_update" [sid upd]
  (let [out (or (content-text (get upd "content"))
                (let [raw (get upd "rawOutput")] (when (string? raw) raw)))
        status (kw (get upd "status"))
        kind (kw (get upd "kind"))]
    (cond-> {:event :ling/tool-update :session sid :tool-id (get upd "toolCallId")}
      status            (assoc :status status)
      out               (assoc :output out)
      (get upd "title") (assoc :title (get upd "title"))
      kind              (assoc :kind kind))))

(defmethod update->event "plan" [sid upd]
  (let [entries (mapv (fn [e] {:content (str (get e "content" ""))
                               :priority (or (kw (get e "priority")) :medium)
                               :status (or (kw (get e "status")) :pending)})
                      (filter map? (items (get upd "entries"))))]
    {:event :ling/plan :session sid :entries entries :progress (plan-progress entries)}))

(defmethod update->event "available_commands_update" [sid upd]
  {:event :ling/commands :session sid
   :commands (into [] (keep #(when (map? %) (get % "name"))) (items (get upd "availableCommands")))})

(defmethod update->event "current_mode_update" [sid upd]
  {:event :ling/mode :session sid :mode (get upd "currentModeId")})

(defmethod update->event "usage_update" [sid upd]
  {:event :ling/usage :session sid :usage (or (->usage upd) {})})

(defmethod update->event :default [sid upd]
  {:event :ling/unknown :session sid :kind (get upd "sessionUpdate") :raw upd})

(defn notification->event
  "The event for a session/update notification FRAME, or nil for any other
   notification."
  [frame]
  (when (= "session/update" (get frame "method"))
    (let [params (get frame "params")
          upd (when (map? params) (get params "update"))]
      (when (map? upd) (update->event (get params "sessionId") upd)))))

(defn prompt-result->event
  "The :ling/turn-end event for the RESULT of a session/prompt on SID. Usage
   is read from result.usage or result._meta.usage."
  [sid result]
  (let [usage (when (map? result)
                (or (->usage (get result "usage"))
                    (->usage (get-in result ["_meta" "usage"]))))]
    (cond-> {:event :ling/turn-end :session sid
             :stop-reason (or (when (map? result) (kw (get result "stopReason"))) :end-turn)}
      usage (assoc :usage usage))))

;; =============================================================================
;; Agent -> client requests
;; =============================================================================

(def permission-policies #{:deny :allow})

(def method-not-found -32601)

(defn permission-reply
  "The result for a session/request_permission PARAMS under POLICY (:deny or
   :allow): the first option whose kind matches, else a cancelled outcome."
  [policy params]
  (let [kinds (if (= :allow policy) ["allow_once" "allow_always"] ["reject_once" "reject_always"])
        options (filter map? (when (map? params) (get params "options")))
        chosen (some (fn [k] (some #(when (= k (get % "kind")) %) options)) kinds)]
    (if chosen
      {"outcome" {"outcome" "selected" "optionId" (get chosen "optionId")}}
      {"outcome" {"outcome" "cancelled"}})))

;; =============================================================================
;; Fold: events -> session summary
;; =============================================================================

(def empty-summary
  {:status :idle
   :turns 0
   :text ""
   :tools {}
   :tool-order []
   :plan []
   :progress {:done 0 :total 0}
   :usage {}
   :last-usage nil
   :stop-reason nil})

(def ^:private additive-usage
  #{:input-tokens :output-tokens :cached-read-tokens :cached-write-tokens
    :total-tokens :cost-usd})

(defn add-usage
  "TOTAL with turn USAGE added: token and cost counters sum, context gauges
   take the latest value."
  [total usage]
  (reduce-kv (fn [acc k v]
               (if (additive-usage k) (update acc k (fnil + 0) v) (assoc acc k v)))
             (or total {}) (or usage {})))

(defn- remember-tool [order id]
  (if (some #{id} order) order (conj order id)))

(defn resolve-tool-id
  "The tool a tool-update with TOOL-ID refers to: TOOL-ID itself when it is a
   non-blank string, else nil. dirge 1.0.4 sent completions with an empty id;
   BuddhiLW/dirge#27 keeps the real id, so a blank id is no longer guessed
   onto the most recent open call and the update is dropped instead."
  [tool-id]
  (when (and (string? tool-id) (not (str/blank? tool-id)))
    tool-id))

(defmulti step
  "SUMMARY after EVENT."
  (fn [_summary event] (:event event)))

(defmethod step :ling/text [s {:keys [text]}]
  (-> s (update :text str text) (assoc :status :running)))

(defmethod step :ling/tool-call [s {:keys [tool-id] :as e}]
  (-> s
      (update :tools assoc tool-id (select-keys e [:tool-id :title :kind :status :input :output]))
      (update :tool-order remember-tool tool-id)
      (assoc :status :running)))

(defmethod step :ling/tool-update [s {:keys [tool-id] :as e}]
  (if-let [id (resolve-tool-id tool-id)]
    (-> s
        (update :tools update id merge
                (assoc (select-keys e [:title :kind :status :output]) :tool-id id))
        (update :tool-order remember-tool id))
    s))

(defmethod step :ling/plan [s {:keys [entries progress]}]
  (assoc s :plan entries :progress progress))

(defmethod step :ling/usage [s {:keys [usage]}]
  (update s :usage merge (select-keys usage [:context-used :context-size])))

(defmethod step :ling/turn-end [s {:keys [stop-reason usage]}]
  (-> s
      (assoc :status :idle :stop-reason stop-reason :last-usage usage)
      (update :turns inc)
      (update :usage add-usage usage)))

(defmethod step :default [s _] s)

(defn begin-turn
  "SUMMARY as a new prompt is sent: running, previous stop-reason cleared."
  [summary]
  (assoc summary :status :running :stop-reason nil))

(defn summarize
  "Fold EVENTS into a summary (from empty-summary unless SUMMARY given)."
  ([events] (summarize empty-summary events))
  ([summary events] (reduce step summary events)))

(defn cost-usd
  "Accumulated USD cost of SUMMARY, or nil when the agent reported none."
  [summary]
  (get-in summary [:usage :cost-usd]))

(defn progress-fraction
  "Plan progress of SUMMARY in [0,1], or nil without a plan."
  [{{:keys [done total]} :progress}]
  (when (pos? (or total 0)) (/ (double done) total)))
