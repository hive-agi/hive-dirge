(ns hive-dirge.ling.backend-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.protocol :as addon]
            [hive-dirge.ling.acp :as acp]
            [hive-dirge.ling.addon :as ling-addon]
            [hive-dirge.ling.backend :as b]
            [hive-dirge.ling.domain :as d]
            [hive-dirge.ling.fake-agent :as fa]
            [hive-dirge.ling.ports :as p]
            [hive-spi.addon.headless :as h]))

;; SPDX-License-Identifier: MIT

(def sid "sess-1")

(defn- agent-handlers [{:keys [init-error?]}]
  {"initialize"     (fn [ag f]
                      (if init-error?
                        (fa/deliver! ag {"jsonrpc" "2.0" "id" (get f "id")
                                         "error" {"code" -32000 "message" "boom"}})
                        (fa/reply! ag f {"protocolVersion" 1 "agentInfo" {"name" "fake-dirge"}})))
   "session/new"    (fn [ag f] (fa/reply! ag f {"sessionId" sid}))
   "session/prompt" (fn [ag f] (swap! (:state ag) assoc :held f))
   "session/cancel" (fn [ag _]
                      (when-let [held (:held @(:state ag))]
                        (swap! (:state ag) dissoc :held)
                        (fa/reply! ag held {"stopReason" "cancelled"})))})

(defn- finish-turn! [ag]
  (let [held (:held @(:state ag))]
    (swap! (:state ag) dissoc :held)
    (fa/update! ag sid {"sessionUpdate" "plan"
                        "entries" [{"content" "a" "status" "completed"}
                                   {"content" "b" "status" "pending"}]})
    (fa/reply! ag held {"stopReason" "end_turn"
                        "_meta" {"usage" {"inputTokens" 3 "outputTokens" 4 "costUsd" 0.25}}})))

(defn- harness
  ([] (harness {}))
  ([agent-opts]
   (let [agents (atom [])
         events (atom [])
         backend (b/dirge-backend
                  {:make-session (fn [opts]
                                   (let [ag (fa/scripted-agent (agent-handlers agent-opts))]
                                     (swap! agents conj {:agent ag :opts opts})
                                     (acp/acp-session ag opts)))
                   :defaults {:timeout-ms 1000 :permission :deny}
                   :on-event (fn [id e] (swap! events conj [id (:event e)]))})]
     {:backend backend :agents agents :events events})))

(defn- agent-of [hx] (:agent (last @(:agents hx))))

(defn- methods-sent [ag] (mapv #(get % "method") (fa/received ag)))

(defn- refusal [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo refused (ex-data refused))))

(defn- status-of [backend id] (h/headless-status backend {:id id} nil))

;; =============================================================================
;; Lifecycle over a scripted dirge
;; =============================================================================

(deftest backend-satisfies-the-headless-contract
  (let [{:keys [backend]} (harness)]
    (is (h/headless-backend? backend))
    (is (= :dirge (h/headless-id backend)))
    (is (= b/capabilities (h/capabilities backend)))))

(deftest spawn-without-task-opens-an-idle-session
  (let [{:keys [backend] :as hx} (harness)]
    (is (= "l1" (h/headless-spawn! backend {:id "l1" :cwd "/tmp/w"} {})))
    (is (= ["initialize" "session/new"] (methods-sent (agent-of hx))))
    (is (= "/tmp/w" (get-in (last @(:agents hx)) [:opts :cwd])))
    (is (= {:slave/id "l1" :slave/status :idle :ling/spawn-mode :dirge}
           (select-keys (status-of backend "l1") [:slave/id :slave/status :ling/spawn-mode])))))

(deftest spawn-with-task-runs-a-turn-to-completion
  (let [{:keys [backend events] :as hx} (harness)
        ag (do (h/headless-spawn! backend {:id "l1"} {:task "do it"}) (agent-of hx))]
    (is (= "session/prompt" (last (methods-sent ag))))
    (is (= :working (:slave/status (status-of backend "l1"))))
    (finish-turn! ag)
    (let [st (status-of backend "l1")]
      (is (= :idle (:slave/status st)))
      (is (= 1 (:dirge/turns st)))
      (is (= :end-turn (:dirge/stop-reason st)))
      (is (= 0.25 (:ling/cost-usd st)))
      (is (= 0.5 (:ling/progress st))))
    (is (every? #(= "l1" (first %)) @events))
    (is (= [:ling/plan :ling/turn-end] (mapv second @events)))))

(deftest dispatch-sends-one-prompt-per-turn
  (let [{:keys [backend] :as hx} (harness)
        _ (h/headless-spawn! backend {:id "l1"} {})
        ag (agent-of hx)]
    (is (true? (h/headless-dispatch! backend {:id "l1"} {:task "one"})))
    (testing "a second dispatch while the turn runs is refused as busy"
      (is (= :ling/busy (:error (refusal #(h/headless-dispatch! backend {:id "l1"} {:task "two"}))))))
    (finish-turn! ag)
    (is (true? (h/headless-dispatch! backend {:id "l1"} {:task "two"})))
    (is (= 2 (count (filter #{"session/prompt"} (methods-sent ag)))))))

(deftest dispatch-refusals-carry-an-error-keyword
  (let [{:keys [backend]} (harness)]
    (is (= {:ling-id "nope" :error :ling/unknown-session}
           (refusal #(h/headless-dispatch! backend {:id "nope"} {:task "x"}))))
    (h/headless-spawn! backend {:id "l1"} {})
    (is (= :ling/no-task (:error (refusal #(h/headless-dispatch! backend {:id "l1"} {:task "  "})))))))

(deftest interrupt-cancels-the-running-turn
  (let [{:keys [backend] :as hx} (harness)]
    (h/headless-spawn! backend {:id "l1"} {:task "long"})
    (is (= {:success? true :ling-id "l1"} (h/headless-interrupt! backend {:id "l1"})))
    (is (= "session/cancel" (last (methods-sent (agent-of hx)))))
    (let [st (status-of backend "l1")]
      (is (= :idle (:slave/status st)))
      (is (= :cancelled (:dirge/stop-reason st))))
    (is (= {:success? false :ling-id "nope" :reason :ling/unknown-session}
           (h/headless-interrupt! backend {:id "nope"})))))

(deftest kill-closes-the-session-once
  (let [{:keys [backend events] :as hx} (harness)]
    (h/headless-spawn! backend {:id "l1"} {})
    (is (= {:killed? true :id "l1"} (h/headless-kill! backend {:id "l1"})))
    (is (false? (p/alive? (agent-of hx))))
    (is (some #{["l1" :ling/closed]} @events))
    (is (nil? (status-of backend "l1")))
    (is (= {:slave/status :working :dirge/session :unknown}
           (h/headless-status backend {:id "l1"} {:slave/status :working})))
    (is (= {:killed? false :id "l1" :reason :ling/unknown-session}
           (h/headless-kill! backend {:id "l1"})))))

(deftest spawn-refusals-leave-nothing-behind
  (testing "a session that fails to open is closed and not kept"
    (let [{:keys [backend] :as hx} (harness {:init-error? true})]
      (is (= :acp/rpc-error (:error (refusal #(h/headless-spawn! backend {:id "l1"} {})))))
      (is (nil? (b/session backend "l1")))
      (is (false? (p/alive? (agent-of hx))))))
  (testing "a second spawn of the same ling is refused"
    (let [{:keys [backend]} (harness)]
      (h/headless-spawn! backend {:id "l1"} {})
      (is (= :ling/already-spawned (:error (refusal #(h/headless-spawn! backend {:id "l1"} {})))))))
  (testing "a spawn without an id is refused"
    (let [{:keys [backend]} (harness)]
      (is (= :ling/no-id (:error (refusal #(h/headless-spawn! backend {:id nil} {}))))))))

(deftest close-all-closes-every-session
  (let [{:keys [backend agents]} (harness)]
    (h/headless-spawn! backend {:id "a"} {})
    (h/headless-spawn! backend {:id "b"} {})
    (is (= #{"a" "b"} (set (b/close-all! backend))))
    (is (every? (comp false? p/alive? :agent) @agents))
    (is (= [] (b/close-all! backend)))))

;; =============================================================================
;; Pure
;; =============================================================================

(deftest session-opts-layers-ctx-and-opts-over-defaults
  (let [seen (atom nil)
        opts (b/session-opts {:cwd "/d" :bin "dirge" :env {"A" "1"}}
                             {:id "l1" :cwd "/w"}
                             {:env-extra {"B" "2"} :permission :allow :task "ignored"}
                             (fn [id e] (reset! seen [id e])))]
    (is (= {:cwd "/w" :bin "dirge" :env {"A" "1" "B" "2"} :permission :allow}
           (dissoc opts :on-event)))
    ((:on-event opts) {:event :ling/text})
    (is (= ["l1" {:event :ling/text}] @seen))
    (is (= {:cwd "/d"} (b/session-opts {:cwd "/d"} {:id "l1"} {} nil)))))

(defspec slave-status-is-total-and-dead-only-when-closed 100
  (prop/for-all [closed gen/boolean
                 status (gen/elements [:idle :running :other nil])]
    (let [s (b/slave-status closed {:status status})]
      (and (contains? #{:dead :working :idle} s)
           (= closed (= :dead s))
           (= (and (not closed) (= :running status)) (= :working s))))))

(defspec status-cost-is-the-sum-of-turn-costs 50
  (prop/for-all [cents (gen/vector (gen/choose 0 500) 1 8)]
    (let [events (mapv (fn [c] {:event :ling/turn-end :stop-reason :end-turn
                                :usage {:cost-usd (/ c 100.0)}})
                       cents)
          st (b/status-map "l" (d/summarize events) events)]
      (and (= (count cents) (:dirge/turns st))
           (< (Math/abs (- (:ling/cost-usd st) (/ (reduce + cents) 100.0))) 1e-9)
           (= :idle (:slave/status st))))))

;; =============================================================================
;; Addon and manifest
;; =============================================================================

(defn- scripted-session [opts]
  (acp/acp-session (fa/scripted-agent (agent-handlers {})) opts))

(deftest addon-registers-and-deregisters-the-backend
  (let [calls (atom [])
        a (ling-addon/addon-ctor
           {:ling/register! (fn [id be meta]
                              (swap! calls conj [:register id (h/headless-backend? be) meta])
                              {:registered? true :headless-id id})
            :ling/deregister! (fn [id] (swap! calls conj [:deregister id]))
            :ling/make-session scripted-session})
        init (addon/initialize! a {})
        be ((:ling/backend (addon/hooks a)))]
    (is (:success? init))
    (is (= {:headless-id :dirge :registered? true} (:metadata init)))
    (is (:already-initialized? (addon/initialize! a {})))
    (h/headless-spawn! be {:id "l1"} {})
    (is (= :ok (:status (addon/health a))))
    (is (= 1 (get-in (addon/health a) [:details :sessions])))
    (addon/shutdown! a)
    (is (= [[:register :dirge true {:provides #{:dirge} :priority 5}]
            [:deregister :dirge]]
           @calls))
    (is (nil? (b/session be "l1")))
    (is (= {} (addon/hooks a)))))

(deftest addon-degrades-when-the-registry-refuses
  (let [deregistered (atom 0)
        a (ling-addon/addon-ctor
           {:ling/register! (fn [_ _ _] {:registered? false :errors ["rejected"]})
            :ling/deregister! (fn [_] (swap! deregistered inc))
            :ling/priority 1})
        init (addon/initialize! a {})]
    (is (:success? init))
    (is (false? (get-in init [:metadata :registered?])))
    (is (= {:status :degraded
            :details {:lifecycle :active :sessions 0 :registered? false :errors ["rejected"]}}
           (addon/health a)))
    (addon/shutdown! a)
    (is (zero? @deregistered))))

(deftest addon-degrades-when-no-host-is-on-the-classpath
  (when-not (io/resource "hive_mcp/agent/ling/headless_registry.clj")
    (let [a (ling-addon/addon-ctor {})
          init (addon/initialize! a {})]
      (is (:success? init))
      (is (false? (get-in init [:metadata :registered?])))
      (is (= :degraded (:status (addon/health a))))
      (addon/shutdown! a))))

(deftest manifest-resolves-to-the-ling-addon
  (let [m (edn/read-string (slurp (io/resource "META-INF/hive-addons/hive-dirge-ling.edn")))
        ctor (requiring-resolve (symbol (:addon/init-ns m) (:addon/init-fn m)))
        a (ctor (:addon/config m))]
    (is (= (:addon/id m) (addon/addon-id a)))
    (is (= (:addon/capabilities m) (addon/capabilities a)))
    (is (= #{b/backend-id} (get-in m [:addon/headless-backend :registers-as])))
    (is (= #{b/backend-id} (:provides (ling-addon/registry-meta (:addon/config m)))))))
