(ns hive-dirge.hive.swarm-test
  "The /hive swarm domain: scope config, request builder, row shaping."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dirge.hive.domain :as d]))

(def cfg (d/resolve-config nil))

(deftest swarm-scope-config
  (is (= :all (:hive/swarm-scope cfg)))
  (is (= :project (:hive/swarm-scope (d/resolve-config {:addon/config {:hive/swarm-scope :project}}))))
  (is (= :project (:hive/swarm-scope (d/resolve-config {:hive/swarm-scope "project"}))))
  (is (= :all (:hive/swarm-scope (d/resolve-config {:hive/swarm-scope "all"}))))
  (is (= "hive-mcp" (:hive/swarm-scope (d/resolve-config {:hive/swarm-scope "hive-mcp"}))))
  (testing "invalid falls back to :all"
    (is (= :all (:hive/swarm-scope (d/resolve-config {:hive/swarm-scope 3}))))
    (is (= :all (:hive/swarm-scope (d/resolve-config {:hive/swarm-scope " "}))))))

(deftest swarm-parse
  (is (= {:action :lens :lens "swarm"} (d/parse-command {:argv ["swarm"]})))
  (is (= {:action :lens :lens "swarm" :scope :project} (d/parse-command {:args "swarm project"})))
  (is (= {:action :lens :lens "swarm" :scope "hive-mcp"} (d/parse-command {:args "swarm hive-mcp"}))))

(deftest swarm-request-lists-all-by-default
  (testing "default scope sends no project_id and the registry-wide agent_id"
    (is (= ["hive" "swarm" {"command" "agent status" "agent_id" "coordinator"}]
           (d/swarm-request cfg "/w/proj" nil))))
  (testing "explicit :all overrides a narrower config"
    (is (= {"command" "agent status" "agent_id" "coordinator"}
           (nth (d/swarm-request (assoc cfg :hive/swarm-scope :project) "/w/proj" :all) 2))))
  (testing ":project narrows to the directory's project"
    (is (= "proj" (get (nth (d/swarm-request cfg "/w/proj" :project) 2) "project_id"))))
  (testing "a project id string narrows to it"
    (is (= "hive-mcp" (get (nth (d/swarm-request cfg "/w/proj" "hive-mcp") 2) "project_id")))))

(deftest swarm-rows-shapes
  (is (= [{:id "a"}] (d/swarm-rows {:agents [{:id "a"} 3]})))
  (is (= [{:id "self"}] (d/swarm-rows {:agent {:id "self"}})))
  (is (= [{:id "b"}] (d/swarm-rows [{:id "b"}])))
  (is (nil? (d/swarm-rows "text")))
  (is (nil? (d/swarm-rows {:other 1}))))

(deftest swarm-working-first
  (is (= ["a" "c" "b" "d"]
         (map :id (d/working-first [{:id "d" :status "idle"} {:id "c" :status "working"}
                                    {:id "b" :status "blocked"} {:id "a" :status "working"}]))))
  (is (= [] (d/working-first nil)))
  (testing "scope parsing keeps working next to memory and shout"
    (is (= {:action :lens :lens "swarm" :scope :all} (d/parse-command {:args "swarm all"})))
    (is (= {:action :memory :query "swarm"} (d/parse-command {:args "memory swarm"})))))

(deftest swarm-panel-and-summary
  (let [rows [{:id "l1" :status "working" :project-id "hive-dirge"}
              {:id "l2" :status "idle" :project-id nil}]
        p    (d/swarm-panel rows "" :all)]
    (is (= {:op :show :id "hive-swarm" :title "hive swarm (all)"} (dissoc p :lines)))
    (is (= [{:text "l1  working  hive-dirge" :id "l1" :face "normal"}
            {:text "l2  idle  -" :id "l2" :face "dim"}]
           (:lines p)))
    (is (= "hive swarm: 2 agents, 1 working (side panel)" (d/swarm-summary rows))))
  (is (= [{:text "no agents" :face "dim"}] (:lines (d/swarm-panel [] "" :project))))
  (is (= "raw" (:markdown (d/swarm-panel nil "raw" "x"))))
  (is (= "hive swarm shown in the side panel" (d/swarm-summary nil))))
