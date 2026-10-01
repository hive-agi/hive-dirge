(ns hive-dirge.ling.domain-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dirge.ling.domain :as d]
            [hive-dirge.ling.fake-agent :as fake]))

;; SPDX-License-Identifier: MIT

(def recording (fake/load-recording "test/fixtures/acp/dirge-1.0.4-tool-turn.jsonl"))

(defn- incoming [kind]
  (->> recording (filter #(= :in (first %))) (map second)
       (filter #(= kind (d/frame-kind %)))))

(defn- recorded-events []
  (let [prompt-id (some (fn [[dir f]] (when (and (= :out dir) (= "session/prompt" (get f "method")))
                                        (get f "id")))
                        recording)
        sid (get-in (first (filter #(get-in % ["result" "sessionId"]) (incoming :response)))
                    ["result" "sessionId"])
        turn-end (some #(when (= prompt-id (get % "id")) (d/prompt-result->event sid (get % "result")))
                       (incoming :response))]
    (conj (vec (keep d/notification->event (incoming :notification))) turn-end)))

;; =============================================================================
;; Recorded dirge 1.0.4 frames
;; =============================================================================

(deftest recorded-frames-classify
  (is (= [:request :request :request] (->> recording (filter #(= :out (first %)))
                                            (map (comp d/frame-kind second)))))
  (is (every? #{:response :notification} (->> recording (filter #(= :in (first %)))
                                              (map (comp d/frame-kind second))))))

(deftest recorded-frames-map-to-events
  (let [events (recorded-events)]
    (is (= [:ling/commands :ling/tool-call :ling/tool-update :ling/tool-update
            :ling/text :ling/turn-end]
           (mapv :event events)))
    (testing "tool call carries its title and raw input"
      (let [tc (second events)]
        (is (= "bash" (:title tc)))
        (is (= "echo acp-probe" (get-in tc [:input "command"])))
        (is (= :pending (:status tc)))))
    (testing "the completion arrives with the output text"
      (is (= "acp-probe\n" (:output (nth events 3))))
      (is (= :completed (:status (nth events 3)))))
    (is (= :end-turn (:stop-reason (last events))))))

(deftest recorded-turn-folds-to-summary
  (let [s (d/summarize (recorded-events))
        [tool-id] (:tool-order s)]
    (is (= 1 (:turns s)))
    (is (= :idle (:status s)))
    (is (= "done" (:text s)))
    (testing "an empty-id completion lands on the open call"
      (is (= [tool-id] (:tool-order s)))
      (is (not (contains? (:tools s) "")))
      (is (= :completed (get-in s [:tools tool-id :status])))
      (is (= "acp-probe\n" (get-in s [:tools tool-id :output]))))))

;; =============================================================================
;; Frames
;; =============================================================================

(deftest outgoing-frames
  (is (= {"jsonrpc" "2.0" "id" 3 "method" "session/prompt"
          "params" {"sessionId" "s" "prompt" [{"type" "text" "text" "hi"}]}}
         (d/prompt-request 3 "s" "hi")))
  (is (= "session/cancel" (get (d/cancel-notification "s") "method")))
  (is (not (contains? (d/cancel-notification "s") "id")))
  (is (= 1 (get-in (d/initialize-request 0) ["params" "protocolVersion"]))))

(deftest frame-kinds
  (is (= :response (d/frame-kind {"id" 1 "result" nil})))
  (is (= :response (d/frame-kind {"id" 1 "error" {"code" 1}})))
  (is (= :request (d/frame-kind {"id" 1 "method" "x"})))
  (is (= :notification (d/frame-kind {"method" "x"})))
  (is (= :invalid (d/frame-kind {"id" 1})))
  (is (= :invalid (d/frame-kind "nope"))))

(deftest response-outcomes
  (is (= {:ok {"a" 1}} (d/response-outcome {"id" 1 "result" {"a" 1}})))
  (is (= {:error :acp/rpc-error :code -32000 :message "boom" :data nil}
         (d/response-outcome {"id" 1 "error" {"code" -32000 "message" "boom"}}))))

;; =============================================================================
;; Updates the recording does not cover (ACP schema shapes)
;; =============================================================================

(deftest plan-update
  (let [e (d/update->event "s" {"sessionUpdate" "plan"
                                "entries" [{"content" "a" "priority" "high" "status" "completed"}
                                           {"content" "b" "priority" "low" "status" "in_progress"}
                                           {"content" "c" "status" "pending"}]})]
    (is (= :ling/plan (:event e)))
    (is (= {:done 1 :total 3} (:progress e)))
    (is (= [:completed :in-progress :pending] (mapv :status (:entries e))))
    (is (= :medium (:priority (nth (:entries e) 2))))
    (is (< (Math/abs (- (/ 1.0 3) (d/progress-fraction (d/step d/empty-summary e)))) 1e-9))
    (is (nil? (d/progress-fraction d/empty-summary)))))

(deftest usage-shapes
  (testing "prompt result _meta.usage, camelCase"
    (is (= {:input-tokens 10 :output-tokens 5 :cost-usd 0.25}
           (:usage (d/prompt-result->event "s" {"stopReason" "end_turn"
                                                "_meta" {"usage" {"inputTokens" 10
                                                                  "outputTokens" 5
                                                                  "costUsd" 0.25}}})))))
  (testing "result.usage, snake_case"
    (is (= {:input-tokens 1 :cached-read-tokens 2}
           (:usage (d/prompt-result->event "s" {"usage" {"input_tokens" 1
                                                         "cached_read_tokens" 2}})))))
  (testing "usage_update notification"
    (is (= {:context-used 100 :context-size 1000 :cost-usd 0.5}
           (:usage (d/update->event "s" {"sessionUpdate" "usage_update" "used" 100 "size" 1000
                                         "cost" {"amount" 0.5 "currency" "USD"}})))))
  (testing "non-USD cost is not read as dollars"
    (is (nil? (:cost-usd (d/->usage {"cost" {"amount" 3 "currency" "EUR"}})))))
  (is (nil? (d/->usage {"note" "no numbers"})))
  (is (not (contains? (d/prompt-result->event "s" {"stopReason" "cancelled"}) :usage))))

(deftest usage-accumulates-across-turns
  (let [s (d/summarize [{:event :ling/turn-end :stop-reason :end-turn
                         :usage {:input-tokens 10 :cost-usd 0.1}}
                        {:event :ling/usage :usage {:context-used 50 :context-size 100}}
                        {:event :ling/turn-end :stop-reason :cancelled
                         :usage {:input-tokens 5 :cost-usd 0.2}}])]
    (is (= 2 (:turns s)))
    (is (= 15 (get-in s [:usage :input-tokens])))
    (is (< (Math/abs (- 0.3 (d/cost-usd s))) 1e-9))
    (is (= 50 (get-in s [:usage :context-used])))
    (is (= :cancelled (:stop-reason s)))))

(deftest content-blocks
  (is (= "ab" (:text (d/update->event "s" {"sessionUpdate" "agent_message_chunk"
                                           "content" [{"type" "text" "text" "a"}
                                                      {"type" "image" "data" "x"}
                                                      {"type" "text" "text" "b"}]}))))
  (is (= "" (:text (d/update->event "s" {"sessionUpdate" "agent_thought_chunk"}))))
  (is (= "file:///x" (:output (d/update->event "s" {"sessionUpdate" "tool_call_update"
                                                     "toolCallId" "t"
                                                     "content" [{"type" "resource_link"
                                                                 "uri" "file:///x"}]})))))

(deftest unknown-update-is-kept
  (let [e (d/update->event "s" {"sessionUpdate" "future_thing" "x" 1})]
    (is (= :ling/unknown (:event e)))
    (is (= "future_thing" (:kind e)))
    (is (= d/empty-summary (d/step d/empty-summary e)))))

(deftest non-array-fields-do-not-throw
  (is (= {:done 0 :total 0} (:progress (d/update->event "s" {"sessionUpdate" "plan" "entries" 0}))))
  (is (= [] (:commands (d/update->event "s" {"sessionUpdate" "available_commands_update"
                                             "availableCommands" {"name" "x"}}))))
  (is (= "" (:text (d/update->event "s" {"sessionUpdate" "agent_message_chunk" "content" 5})))))

(deftest non-update-notifications-map-to-nil
  (is (nil? (d/notification->event {"method" "session/other" "params" {}})))
  (is (nil? (d/notification->event {"method" "session/update" "params" {"sessionId" "s"}}))))

(deftest blank-id-update-without-open-call-is-dropped
  (let [s (d/step (d/summarize [{:event :ling/tool-call :tool-id "a" :status :pending}
                                {:event :ling/tool-update :tool-id "a" :status :completed}])
                  {:event :ling/tool-update :tool-id "" :status :completed :output "x"})]
    (is (= ["a"] (:tool-order s)))
    (is (nil? (get-in s [:tools "a" :output])))))

(deftest permission-replies
  (let [params {"options" [{"optionId" "y" "kind" "allow_once"}
                           {"optionId" "n" "kind" "reject_once"}]}]
    (is (= "n" (get-in (d/permission-reply :deny params) ["outcome" "optionId"])))
    (is (= "y" (get-in (d/permission-reply :allow params) ["outcome" "optionId"])))
    (is (= {"outcome" {"outcome" "cancelled"}}
           (d/permission-reply :deny {"options" [{"optionId" "y" "kind" "allow_once"}]})))
    (is (= {"outcome" {"outcome" "cancelled"}} (d/permission-reply :deny nil)))))

;; =============================================================================
;; Properties
;; =============================================================================

(def gen-scalar (gen/one-of [gen/string-alphanumeric gen/small-integer gen/boolean
                             (gen/return nil) gen/double]))

(def gen-json
  (gen/recursive-gen (fn [inner] (gen/one-of [(gen/vector inner 0 3)
                                              (gen/map gen/string-alphanumeric inner {:max-elements 3})]))
                     gen-scalar))

(def update-kinds
  ["agent_message_chunk" "agent_thought_chunk" "user_message_chunk" "tool_call"
   "tool_call_update" "plan" "available_commands_update" "current_mode_update"
   "usage_update" "something_new"])

(def field-names
  ["content" "toolCallId" "title" "kind" "status" "rawInput" "rawOutput" "entries"
   "availableCommands" "currentModeId" "used" "size" "cost"])

(def gen-update
  (gen/let [kind (gen/elements update-kinds)
            fields (gen/map (gen/elements field-names) gen-json {:max-elements 5})]
    (assoc fields "sessionUpdate" kind)))

(defspec any-update-maps-to-one-event 200
  (prop/for-all [upd gen-update]
    (let [e (d/notification->event {"jsonrpc" "2.0" "method" "session/update"
                                    "params" {"sessionId" "s" "update" upd}})]
      (and (keyword? (:event e)) (= "s" (:session e))))))

(def gen-event
  (gen/one-of
   [(gen/fmap #(hash-map :event :ling/text :text %) gen/string-alphanumeric)
    (gen/fmap #(hash-map :event :ling/tool-call :tool-id % :status :pending)
              (gen/elements ["a" "b" "c"]))
    (gen/let [id (gen/elements ["a" "b" "c" ""]) st (gen/elements [:in-progress :completed :failed])]
      {:event :ling/tool-update :tool-id id :status st})
    (gen/let [n gen/nat c (gen/elements [0.0 0.5 1.25])]
      {:event :ling/turn-end :stop-reason :end-turn :usage {:input-tokens n :cost-usd c}})
    (gen/return {:event :ling/turn-end :stop-reason :error})]))

(defspec fold-invariants 200
  (prop/for-all [events (gen/vector gen-event 0 30)]
    (let [s (d/summarize events)
          ends (filter #(= :ling/turn-end (:event %)) events)]
      (and (= (count ends) (:turns s))
           (apply distinct? nil (:tool-order s))
           (= (set (:tool-order s)) (set (keys (:tools s))))
           (not (contains? (:tools s) ""))
           (= (reduce + 0 (keep #(get-in % [:usage :input-tokens]) ends))
              (get-in s [:usage :input-tokens] 0))
           (= (apply str (keep #(when (= :ling/text (:event %)) (:text %)) events))
              (:text s))))))
