(ns hive-dirge.hive.domain-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dirge.hive.domain :as d]))

(def cfg (d/resolve-config nil))

(deftest resolve-config-defaults-and-overrides
  (is (= d/default-config (d/resolve-config nil)))
  (is (= "hive" (:hive/server (d/resolve-config {:addon/config {}}))))
  (testing "dirge initialize! shape"
    (is (= "hive-dev" (:hive/server (d/resolve-config {:addon/id "hive.dirge"
                                                         :addon/config {:hive/server "hive-dev"}})))))
  (testing "bare map"
    (is (= 7 (:hive/kanban-limit (d/resolve-config {:hive/kanban-limit 7})))))
  (testing "invalid values fall back"
    (let [c (d/resolve-config {:addon/config {:hive/server "  " :hive/kanban-limit -1
                                              :hive/max-prompt-chars "x"}})]
      (is (= "hive" (:hive/server c)))
      (is (= 50 (:hive/kanban-limit c)))
      (is (= 60000 (:hive/max-prompt-chars c))))))

(deftest parse-command-cases
  (is (= {:action :help} (d/parse-command {:args "" :argv []})))
  (is (= {:action :help} (d/parse-command {})))
  (is (= {:action :catchup} (d/parse-command {:args "catchup"})))
  (is (= {:action :wrap} (d/parse-command {:argv ["wrap"]})))
  (is (= {:action :lens :lens "kanban"} (d/parse-command {:args " kanban "})))
  (is (= {:action :lens :lens "kanban" :status "todo"} (d/parse-command {:argv ["kanban" "todo"]})))
  (is (= :unknown (:action (d/parse-command {:argv ["kanban" "later"]}))))
  (is (= :unknown (:action (d/parse-command {:args "frobnicate"}))))
  (testing "argv wins over args"
    (is (= {:action :wrap} (d/parse-command {:args "catchup" :argv ["wrap"]})))))

(deftest parse-command-memory-swarm-shout
  (is (= {:action :memory :query "addon reload"} (d/parse-command {:argv ["memory" "addon" "reload"]})))
  (is (= :unknown (:action (d/parse-command {:args "memory"}))))
  (is (= {:action :lens :lens "swarm"} (d/parse-command {:args "swarm"})))
  (is (= {:action :lens :lens "swarm" :scope :all} (d/parse-command {:args "swarm all"})))
  (is (= {:action :shout :message "done here"} (d/parse-command {:args "shout done here"})))
  (is (= :unknown (:action (d/parse-command {:args "shout"})))))

(deftest answer-body-cuts-appended-context-blocks
  (is (= "[1]" (d/answer-body "[1]\n\n---MEMORY---\n{}\n---/MEMORY---\n---FRONTIER---")))
  (is (= "a\n---\nb" (d/answer-body "a\n---\nb")) "a markdown rule is not a block")
  (is (= "" (d/answer-body nil))))

(deftest swarm-and-memory-rendering
  (is (= ["a  working  -" "c  working  x" "b  idle  -"]
         (map :text (:lines (d/swarm-panel [{:id "b" :status "idle"}
                                            {:id "c" :status "working" :project-id "x"}
                                            {:id "a" :status "working"}]
                                           "" :all))))
      "working first, each group by id")
  (is (= [{:text "no agents" :face "dim"}] (:lines (d/swarm-panel [] "" :all))))
  (is (= "hive swarm: 0 agents (side panel)" (d/swarm-summary [])))
  (is (= "no memories for \"q\"" (d/memory-text "q" {:results []})))
  (is (= ["hive" "swarm" {"command" "hivemind shout" "event_type" "progress"
                          "task" "dirge" "message" "m" "directory" "/w"}]
         (d/shout-request cfg "/w" "m"))))

(deftest requests
  (is (= ["hive" "project" {"command" "workflow catchup" "directory" "/p/x"}]
         (d/catchup-request cfg "/p/x")))
  (is (= ["hive" "project" {"command" "session wrap" "directory" "/p/x"}]
         (d/wrap-request cfg "/p/x")))
  (testing "no directory key when cwd is unknown"
    (is (= {"command" "workflow catchup"} (nth (d/catchup-request cfg nil) 2))))
  (testing "configured server name"
    (is (= "other" (first (d/catchup-request (d/resolve-config {:hive/server "other"}) "/p")))))
  (is (= ["hive" "project" {"command" "kanban list" "limit" 50 "directory" "/p" "status" "todo"}]
         (d/kanban-request cfg "/p" "todo")))
  (is (not (contains? (nth (d/kanban-request cfg "/p" "bogus") 2) "status")))
  (is (= ["hive" "memory" {"command" "search" "query" "seal" "limit" 10 "directory" "/p"}]
         (d/memory-search-request cfg "/p" {:query "seal"})))
  (is (= {"command" "search" "query" "q" "limit" 3 "type" "decision"}
         (nth (d/memory-search-request cfg nil {:query "q" :limit 3 :type "decision"}) 2))))

(deftest result-shaping
  (let [ok {:content [{:type "text" :text "a"} {:type "image"} {:type "text" :text "b"}]
            :isError false}]
    (is (nil? (d/result-error ok)))
    (is (= "a\nb" (d/result-text ok))))
  (is (str/includes? (d/result-error nil) "not running inside dirge"))
  (is (= "refused" (d/result-error {:error "refused"})))
  (is (= "boom" (d/result-error {:isError true :content [{:type "text" :text "boom"}]})))
  (is (string? (d/result-error {:isError true :content []})))
  (is (string? (d/result-error "weird"))))

(deftest truncate-and-prompt
  (is (= "abc" (d/truncate "abc" 3)))
  (is (str/starts-with? (d/truncate "abcdef" 3) "abc\n[... 3 characters truncated]"))
  (let [c (d/resolve-config {:hive/max-prompt-chars 5})
        p (d/catchup-prompt c "0123456789")]
    (is (str/includes? p "01234\n[... 5"))
    (is (not (str/includes? p "56789")))))

(deftest command-reply-cases
  (let [ans {:content [{:type "text" :text "CTX"}]}]
    (let [r (d/command-reply cfg :catchup ans)]
      (is (str/includes? (:prompt r) "CTX"))
      (is (str/includes? (:text r) "3 characters")))
    (is (= {:text "CTX"} (d/command-reply cfg :wrap ans)))
    (is (= {:text "hive session wrapped"} (d/command-reply cfg :wrap {:content []}))))
  (let [r (d/command-reply cfg :catchup {:error "mcp-call is unavailable"})]
    (is (nil? (:prompt r)))
    (is (str/includes? (:text r) "/hive catchup failed: mcp-call is unavailable"))))

(deftest kanban-shaping
  (let [rows [{:id "1" :title "a" :status "todo" :priority "high" :project "hive"}
              {:id "2" :title "b" :status "todo" :priority "low" :project "hive-dirge"}
              "junk"]]
    (is (= 2 (count (d/kanban-rows rows))))
    (is (= 2 (count (d/kanban-rows {:tasks rows}))))
    (is (nil? (d/kanban-rows "text")))
    (is (= "hive-dirge" (d/project-hint "/home/k/PP/hive/hive-dirge/")))
    (let [clean (d/kanban-rows rows)
          panel (d/kanban-panel clean "" "hive-dirge" "todo")]
      (is (= :show (:op panel)))
      (is (= "hive-kanban" (:id panel)))
      (is (= "hive kanban (todo)" (:title panel)))
      (is (= ["[todo] b  2" "[todo] a  1"] (map :text (:lines panel))))
      (is (= ["dim" "warn"] (map :face (:lines panel))))
      (is (= "hive kanban: 2 tasks, 1 in hive-dirge (side panel)"
             (d/kanban-summary clean "hive-dirge")))))
  (is (= "no tasks" (-> (d/kanban-panel [] "" "p" nil) :lines first :text)))
  (is (= {:op :show :id "hive-kanban" :title "hive kanban" :markdown "raw"}
         (d/kanban-panel nil "raw" "p" nil))))

(deftest tool-answer-cases
  (is (= {:content [{:type "text" :text "x"}] :isError false}
         (d/tool-answer {:content [{:type "text" :text "x"}]})))
  (is (= {:content [{:type "text" :text "nope"}] :isError true}
         (d/tool-answer {:error "nope"}))))

(deftest system-prompt-is-static-text
  (testing "auto-catchup on: no instruction to run /hive catchup"
    (let [s (d/system-prompt (d/resolve-config {:hive/server "hv"}))]
      (is (not (str/includes? s "/hive catchup")))
      (is (str/includes? s "automatically"))
      (is (str/includes? s "/hive wrap"))
      (is (str/includes? s "\"hv\""))))
  (testing "auto-catchup off: /hive catchup is announced"
    (let [s (d/system-prompt (d/resolve-config {:hive/auto-catchup? false}))]
      (is (str/includes? s "/hive catchup"))
      (is (not (str/includes? s "automatically"))))))

(deftest session-config
  (is (true? (:hive/auto-catchup? cfg)))
  (is (true? (:hive/auto-wrap? cfg)))
  (is (false? (:hive/wrap-on-swap? cfg)))
  (is (= 12000 (:hive/max-context-chars cfg)))
  (let [c (d/resolve-config {:addon/config {:hive/auto-catchup? false :hive/auto-wrap? "no"
                                            :hive/wrap-on-swap? true :hive/max-context-chars 0}})]
    (is (false? (:hive/auto-catchup? c)))
    (is (true? (:hive/auto-wrap? c)) "non-boolean falls back")
    (is (true? (:hive/wrap-on-swap? c)))
    (is (= 12000 (:hive/max-context-chars c)))))

(deftest session-start-gating
  (let [ctx {:session-id "s" :cwd "/w/p" :first-prompt? true :mcp-servers ["other" "hive"]}]
    (is (= ["hive" "project" {"command" "workflow catchup" "directory" "/w/p"}]
           (d/session-start-request cfg ctx)))
    (is (= (d/catchup-request cfg "/w/p") (d/session-start-request cfg ctx)))
    (testing "server missing"
      (is (nil? (d/session-start-request cfg (assoc ctx :mcp-servers ["other"]))))
      (is (nil? (d/session-start-request cfg (dissoc ctx :mcp-servers))))
      (is (nil? (d/session-start-request (d/resolve-config {:hive/server "hv"}) ctx))))
    (testing "auto-catchup off"
      (is (nil? (d/session-start-request (d/resolve-config {:hive/auto-catchup? false}) ctx))))))

(deftest session-start-context-shaping
  (let [ok (fn [t] {:content [{:type "text" :text t}] :isError false})]
    (is (str/ends-with? (:context (d/session-start-context cfg (ok "CATCHUP"))) "CATCHUP"))
    (is (nil? (d/session-start-context cfg {:error "down"})))
    (is (nil? (d/session-start-context cfg nil)))
    (is (nil? (d/session-start-context cfg (ok "  "))))
    (testing "truncation"
      (let [c   (d/resolve-config {:hive/max-context-chars 10})
            out (:context (d/session-start-context c (ok (apply str (repeat 50 "x")))))]
        (is (str/includes? out "[... 40 characters truncated]"))
        (is (not (str/includes? out (apply str (repeat 11 "x")))))))))

(deftest session-end-reason-gating
  (let [ctx {:session-id "s" :cwd "/w/p"}]
    (is (= ["hive" "project" {"command" "session wrap" "directory" "/w/p"}]
           (d/session-end-request cfg (assoc ctx :reason :exit))))
    (is (nil? (d/session-end-request cfg (assoc ctx :reason :swap))))
    (is (nil? (d/session-end-request cfg ctx)))
    (is (some? (d/session-end-request (d/resolve-config {:hive/wrap-on-swap? true})
                                      (assoc ctx :reason :swap))))
    (is (nil? (d/session-end-request (d/resolve-config {:hive/auto-wrap? false})
                                     (assoc ctx :reason :exit))))))
