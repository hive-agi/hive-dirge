(ns hive-dirge.hive.guard-test
  "The \"guard\" command hook: dirge hook payloads judged by hive's guard tool
   over MCP, failing open on every path that yields no verdict."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.protocol :as p]
            [hive-dirge.hive.addon :as addon]
            [hive-dirge.hive.domain :as d]))

(def payload {:hook_event_name "PreToolUse" :tool_name "Bash" :tool_input {:command "rm -rf /"}})

(def deny {:hookSpecificOutput {:hookEventName            "PreToolUse"
                                :permissionDecision       "deny"
                                :permissionDecisionReason "rule R1"}})

(defn- text-answer [s] {:content [{:type "text" :text s}] :isError false})

(defn- ports
  "Ports whose mcp-call answers `answer` and whose json-parse reads `parsed`
   for any text, recording calls and log lines."
  [calls answer parsed]
  {:mcp-call   (fn [server tool args] (swap! calls conj [:mcp server tool args]) answer)
   :json-parse (fn [text] (swap! calls conj [:json text]) parsed)
   :log!       (fn [level msg] (swap! calls conj [:log level msg]))
   :cwd        (constantly "/w/proj")})

(defn- guard-handler [ports]
  (let [a (doto (addon/make-addon ports) (p/initialize! {:addon/config {:hive/server "hv"}}))]
    (get-in (p/hooks a) [:dirge/command-hooks "guard"])))

(deftest the-request-names-the-dirge-projection-and-a-deadline
  (is (= ["hv" "guard" {"command" "decide" "harness" "dirge" "payload" payload
                        "deadline_ms" d/guard-deadline-ms}]
         (d/guard-request {:hive/server "hv"} payload))))

(deftest a-verdict-is-handed-on-as-hook-json
  (let [calls (atom [])
        h     (guard-handler (ports calls (text-answer "{...}\n\n---HIVEMIND---\n[]")
                                    {:answer deny :enforcing? true}))]
    (is (= deny (h {:payload payload})))
    (testing "the payload reaches the guard tool unchanged, on the configured server"
      (is (= [:mcp "hv" "guard" (last (d/guard-request {:hive/server "hv"} payload))]
             (first @calls))))
    (testing "piggyback blocks are cut before the JSON is read"
      (is (some #{[:json "{...}"]} @calls)))
    (is (not-any? #(= :log (first %)) @calls) "a judged moment logs nothing")))

(deftest every-way-of-getting-no-verdict-allows-and-says-why
  (doseq [[why answer parsed]
          [["not inside dirge"          nil                                          nil]
           ["refused on the event loop" {:error "blocked: waits on the event loop"} nil]
           ["the tool errored"          {:content [{:type "text" :text "boom"}] :isError true} nil]
           ["unreadable JSON"           (text-answer "not json")                     nil]
           ["a guard gap"               (text-answer "{}")                           {:answer {} :gap "no-projection"}]
           ["no hook JSON"              (text-answer "{}")                           {:enforcing? false}]]]
    (testing why
      (let [calls (atom [])
            out   ((guard-handler (ports calls answer parsed)) {:payload payload})]
        (is (= {} out))
        (is (some #(and (= :log (first %)) (= :warn (second %))) @calls))))))

(deftest a-port-that-throws-still-allows
  (let [calls (atom [])
        p     (assoc (ports calls nil nil)
                     :mcp-call (fn [& _] (throw (ex-info "socket closed" {}))))]
    (is (= {} ((guard-handler p) {:payload payload})))
    (is (some #(= :log (first %)) @calls))))

(deftest outside-dirge-the-real-ports-allow
  (is (= {} (addon/guard-hook addon/harness-ports (d/resolve-config nil) {:payload payload}))))
