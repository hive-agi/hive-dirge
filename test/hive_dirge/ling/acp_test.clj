(ns hive-dirge.ling.acp-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dirge.ling.acp :as acp]
            [hive-dirge.ling.fake-agent :as fake]
            [hive-dirge.ling.ports :as p]
            [hive-dsl.result :as r]))

;; SPDX-License-Identifier: MIT

(def recording (fake/load-recording "test/fixtures/acp/dirge-1.0.4-tool-turn.jsonl"))

(def init-result
  {"protocolVersion" 1 "agentCapabilities" {} "agentInfo" {"name" "fake" "version" "0"}})

(defn- open-handlers
  "Handlers for initialize + session/new answering session id SID."
  [sid]
  {"initialize"  (fn [a f] (fake/reply! a f init-result))
   "session/new" (fn [a f] (fake/reply! a f {"sessionId" sid}))})

(defn- session [agent & [opts]]
  (acp/acp-session agent (merge {:cwd "/work" :timeout-ms 2000} opts)))

(defn- event-kinds [s] (mapv :event (p/transcript s)))

;; =============================================================================
;; Replay of the recorded dirge turn
;; =============================================================================

(deftest replayed-dirge-turn
  (let [agent (fake/replay-agent recording)
        s (session agent)
        opened (p/open! s)]
    (is (r/ok? opened))
    (is (= "e15799d2-905d-413a-b3d7-86bd99faf51a" (:session-id (:ok opened))))
    (is (= {"name" "dirge" "version" "1.0.4"} (:agent (:ok opened))))
    (is (= {:ok {:turn 1}} (p/prompt! s "run echo")))
    (let [done (p/collect! s 2000)
          summary (p/events s)
          [tool-id] (:tool-order summary)]
      (is (= :end-turn (:stop-reason (:ok done))))
      (is (= "done" (:text summary)))
      (is (= :completed (get-in summary [:tools tool-id :status])))
      (is (= "acp-probe\n" (get-in summary [:tools tool-id :output]))))
    (testing "the client sent initialize, session/new, session/prompt in order"
      (is (= ["initialize" "session/new" "session/prompt"]
             (mapv #(get % "method") (fake/received agent))))
      (is (= "/work" (get-in (fake/received agent) [1 "params" "cwd"]))))
    (is (= {:ok true} (p/close! s)))
    (is (= {:ok true} (p/close! s)))
    (is (= 1 (count (filter #{:ling/closed} (event-kinds s)))))
    (is (= 1 (:stops @(:state agent))))))

;; =============================================================================
;; Scripted agent
;; =============================================================================

(deftest prompt-before-open-is-refused
  (let [s (session (fake/scripted-agent (open-handlers "s1")))]
    (is (= :ling/not-open (:error (p/prompt! s "x"))))
    (is (= :ling/not-open (:error (p/cancel! s))))
    (is (= :ling/no-turn (:error (p/collect! s 10))))))

(deftest protocol-version-mismatch-fails-open
  (let [agent (fake/scripted-agent
               {"initialize" (fn [a f] (fake/reply! a f (assoc init-result "protocolVersion" 2)))})
        res (p/open! (session agent))]
    (is (= :acp/protocol-version (:error res)))
    (is (not (p/alive? agent)))))

(deftest rpc-error-on-session-new-fails-open
  (let [agent (fake/scripted-agent
               {"initialize" (fn [a f] (fake/reply! a f init-result))
                "session/new" (fn [a f] (fake/deliver! a {"jsonrpc" "2.0" "id" (get f "id")
                                                          "error" {"code" -32000 "message" "no"}}))})]
    (is (= :acp/rpc-error (:error (p/open! (session agent)))))))

(deftest silent-agent-times-out
  (let [res (p/open! (session (fake/scripted-agent {}) {:timeout-ms 50}))]
    (is (= :ling/timeout (:error res)))
    (is (= :initialize (:waiting-for res)))))

(deftest busy-then-cancel
  (let [held (atom nil)
        agent (fake/scripted-agent
               (merge (open-handlers "s1")
                      {"session/prompt" (fn [_ f] (reset! held f))
                       "session/cancel" (fn [a _] (fake/reply! a @held {"stopReason" "cancelled"}))}))
        s (session agent)]
    (p/open! s)
    (is (r/ok? (p/prompt! s "long")))
    (is (= :running (:status (p/events s))))
    (is (= :ling/busy (:error (p/prompt! s "again"))))
    (is (= :ling/timeout (:error (p/collect! s 20))))
    (is (= {:ok true} (p/cancel! s)))
    (is (= :cancelled (:stop-reason (:ok (p/collect! s 1000)))))
    (testing "the next turn may start"
      (is (= {:ok {:turn 2}} (p/prompt! s "again"))))))

(deftest streamed-updates-and-usage
  (let [agent (fake/scripted-agent
               (merge (open-handlers "s1")
                      {"session/prompt"
                       (fn [a f]
                         (fake/update! a "s1" {"sessionUpdate" "plan"
                                               "entries" [{"content" "a" "status" "completed"}
                                                          {"content" "b" "status" "pending"}]})
                         (fake/update! a "s1" {"sessionUpdate" "agent_message_chunk"
                                               "content" {"type" "text" "text" "hi"}})
                         (fake/reply! a f {"stopReason" "end_turn"
                                           "_meta" {"usage" {"inputTokens" 7 "outputTokens" 3
                                                             "costUsd" 0.01}}}))}))
        seen (atom [])
        s (session agent {:on-event #(swap! seen conj (:event %))})]
    (p/open! s)
    (p/prompt! s "go")
    (p/collect! s 1000)
    (is (= {:done 1 :total 2} (:progress (p/events s))))
    (is (= {:input-tokens 7 :output-tokens 3 :cost-usd 0.01} (p/cost s)))
    (is (= [:ling/plan :ling/text :ling/turn-end] @seen))))

(deftest agent-requests-are-answered
  (let [agent (fake/scripted-agent
               (merge (open-handlers "s1")
                      {"session/prompt"
                       (fn [a f]
                         (fake/deliver! a {"jsonrpc" "2.0" "id" 900 "method" "session/request_permission"
                                           "params" {"sessionId" "s1"
                                                     "options" [{"optionId" "ok" "kind" "allow_once"}
                                                                {"optionId" "no" "kind" "reject_once"}]}})
                         (fake/deliver! a {"jsonrpc" "2.0" "id" 901 "method" "fs/read_text_file"
                                           "params" {}})
                         (fake/reply! a f {"stopReason" "end_turn"}))}))
        s (session agent)]
    (p/open! s)
    (p/prompt! s "go")
    (let [answers (into {} (keep #(when (#{900 901} (get % "id")) [(get % "id") %]))
                        (fake/received agent))]
      (is (= "no" (get-in answers [900 "result" "outcome" "optionId"])))
      (is (= -32601 (get-in answers [901 "error" "code"]))))
    (testing ":allow policy picks the allow option"
      (let [agent2 (fake/scripted-agent
                    (merge (open-handlers "s2")
                           {"session/prompt"
                            (fn [a _]
                              (fake/deliver! a {"jsonrpc" "2.0" "id" 5 "method" "session/request_permission"
                                                "params" {"options" [{"optionId" "ok" "kind" "allow_once"}]}}))}))
            s2 (session agent2 {:permission :allow})]
        (p/open! s2)
        (p/prompt! s2 "go")
        (is (= "ok" (some #(when (= 5 (get % "id")) (get-in % ["result" "outcome" "optionId"]))
                          (fake/received agent2))))))))

(deftest transport-closed-mid-turn-fails-the-turn
  (let [agent (fake/scripted-agent (merge (open-handlers "s1") {"session/prompt" (fn [_ _])}))
        s (session agent)]
    (p/open! s)
    (p/prompt! s "go")
    (fake/deliver! agent {:transport/closed {:eof true :exit 1}})
    (let [res (p/collect! s 1000)]
      (is (= :ling/transport-closed (:error res)) "a later collect sees the same error")
      (is (= {:eof true :exit 1} (:reason res)))
      (is (= :error (:stop-reason (last (filter #(= :ling/turn-end (:event %)) (p/transcript s)))))))
    (is (= :ling/not-open (:error (p/prompt! s "again"))))
    (fake/deliver! agent {:transport/closed {:eof true}})
    (p/close! s)
    (is (= 1 (count (filter #{:ling/closed} (event-kinds s)))))))

(deftest collect-sees-the-failed-turn-when-waiting
  (let [agent (fake/scripted-agent (merge (open-handlers "s1") {"session/prompt" (fn [_ _])}))
        s (session agent)]
    (p/open! s)
    (p/prompt! s "go")
    (let [waiting (future (p/collect! s 2000))]
      (fake/deliver! agent {:transport/closed {:eof true}})
      (is (= :ling/transport-closed (:error (deref waiting 3000 nil)))))))

(deftest transport-close-keeps-the-turn-until-it-ends
  (let [agent (fake/scripted-agent (merge (open-handlers "s1") {"session/prompt" (fn [_ _])}))
        s (session agent)]
    (p/open! s)
    (p/prompt! s "go")
    (swap! (:state s) #'acp/fail-pending)
    (is (some? (:turn @(:state s))))
    (is (= :ling/timeout (:error (p/collect! s 20))) "collect! waits instead of answering :ling/no-turn")))

(deftest failed-prompt-send-ends-the-turn
  (let [agent (fake/scripted-agent (open-handlers "s1"))
        s (session agent)]
    (p/open! s)
    (swap! (:state agent) assoc :alive? false)
    (is (r/ok? (p/prompt! s "go")))
    (is (= :ling/not-started (:error (p/collect! s 1000))))
    (is (nil? (:turn @(:state s))))
    (is (not= :ling/busy (:error (p/prompt! s "again"))))))

(deftest wire-errors-and-throwing-listener
  (let [agent (fake/scripted-agent (open-handlers "s1"))
        s (session agent {:on-event (fn [_] (throw (ex-info "listener broke" {})))})]
    (is (r/ok? (p/open! s)))
    (fake/deliver! agent {:transport/error :acp/unparseable-line :line "garbage"})
    (fake/deliver! agent {"jsonrpc" "2.0"})
    (is (= [:ling/wire-error :ling/wire-error] (event-kinds s)))
    (is (= 2 (count (:callback-errors @(:state s)))))
    (is (= "listener broke" (:message (first (:callback-errors @(:state s))))))))

(deftest unmatched-response-is-ignored
  (let [agent (fake/scripted-agent (open-handlers "s1"))
        s (session agent)]
    (p/open! s)
    (fake/deliver! agent {"jsonrpc" "2.0" "id" 424242 "result" {}})
    (is (= [] (event-kinds s)))
    (is (= {} (:pending @(:state s))))))
