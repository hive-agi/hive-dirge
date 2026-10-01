(ns hive-dirge.doctor.domain-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dirge.doctor.domain :as d]))

(def discovery-path "/run/user/1000/hive-vessel/dirge.json")
(def config-path "/h/u/.config/dirge/config.json")

(def live-discovery
  "{\"vessel\":\"dirge\",\"dialect\":\"json\",\"url\":\"http://127.0.0.1:4100/vessel\",\"port\":4100,\"token\":\"t\",\"pid\":42}")

(def good-config "{\"provider\":\"x\",\"panel_feed\":{\"discovery_dir\":\"hive-vessel\"}}")

(def healthy
  {:discovery/path discovery-path
   :discovery/text live-discovery
   :discovery/pid-alive? true
   :discovery/port-open? true
   :dirge/config-path config-path
   :dirge/config-text good-config})

(defn- status [check] (:check/status check))

(deftest host-step-passes-for-a-live-discovery-file
  (let [c (d/check-host healthy)]
    (is (= :pass (status c)))
    (is (= 1 (:check/step c)))
    (is (str/includes? (:check/summary c) "http://127.0.0.1:4100/vessel"))
    (is (nil? (:check/fix c)))))

(deftest host-step-fails
  (testing "no discovery file"
    (let [c (d/check-host (assoc healthy :discovery/text nil))]
      (is (= :fail (status c)))
      (is (str/includes? (:check/summary c) "no discovery file"))
      (is (str/includes? (:check/fix c) "local.deps.edn"))))
  (testing "not JSON"
    (is (str/includes? (:check/summary (d/check-host (assoc healthy :discovery/text "{oops")))
                       "is not JSON")))
  (testing "JSON but not an object"
    (is (= :fail (status (d/check-host (assoc healthy :discovery/text "[1,2]"))))))
  (testing "another vessel's file"
    (is (str/includes? (:check/summary (d/check-host (assoc healthy :discovery/text
                                                            "{\"vessel\":\"vim\",\"port\":1}")))
                       "not for vessel dirge")))
  (testing "stale: pid gone"
    (is (str/includes? (:check/summary (d/check-host (assoc healthy :discovery/pid-alive? false)))
                       "pid 42 is not running")))
  (testing "stale: port closed"
    (is (str/includes? (:check/summary (d/check-host (assoc healthy :discovery/port-open? false)))
                       "port 4100"))))

(deftest host-step-unknown-liveness-is-not-a-failure
  (is (= :pass (status (d/check-host (assoc healthy
                                            :discovery/pid-alive? nil
                                            :discovery/port-open? nil))))))

(deftest host-step-names-the-classpath-when-the-file-is-missing
  (let [missing (assoc healthy :discovery/text nil)
        summary #(:check/summary (d/check-host (assoc missing :hive-mcp/processes %)))]
    (is (not (str/includes? (summary nil) ";")))
    (is (str/includes? (summary []) "no running hive-mcp process"))
    (is (str/includes? (summary [{:pid 7 :hive-dirge? false}]) "do not name hive-dirge"))
    (is (str/includes? (summary [{:pid 7 :hive-dirge? true}]) "check its log"))))

(deftest config-step-passes-with-panel-feed
  (let [c (d/check-dirge-config healthy)]
    (is (= :pass (status c)))
    (is (= 2 (:check/step c)))))

(deftest config-step-fails
  (let [summary #(:check/summary (d/check-dirge-config (assoc healthy :dirge/config-text %)))]
    (is (str/includes? (summary nil) "no dirge config"))
    (is (str/includes? (summary "not json") "is not JSON"))
    (is (str/includes? (summary "\"just a string\"") "not a JSON object"))
    (is (str/includes? (summary "{\"provider\":\"x\"}") "has no panel_feed"))
    (is (str/includes? (summary "{\"panel_feed\":{}}") "discovery_dir is unset"))
    (is (str/includes? (summary "{\"panel_feed\":{\"discovery_dir\":\"elsewhere\"}}")
                       "\"elsewhere\""))
    (is (every? #(= :fail (status (d/check-dirge-config (assoc healthy :dirge/config-text %))))
                [nil "not json" "{}" "{\"panel_feed\":true}"]))))

(deftest discovery-endpoint-reads-pid-and-port
  (is (= {:pid 42 :port 4100} (d/discovery-endpoint live-discovery)))
  (is (nil? (d/discovery-endpoint nil)))
  (is (nil? (d/discovery-endpoint "{oops")))
  (is (nil? (d/discovery-endpoint "[1]"))))

(def discovery-variants
  [nil "" "{oops" "[1]" "{\"vessel\":\"vim\"}" live-discovery])

(def config-variants
  [nil "" "nope" "{}" "{\"panel_feed\":{}}" "{\"panel_feed\":{\"discovery_dir\":\"x\"}}" good-config])

(def all-facts
  (for [dt discovery-variants
        pid [nil true false]
        port [nil true false]
        procs [nil [] [{:pid 1 :hive-dirge? true}]]
        ct config-variants]
    (assoc healthy
           :discovery/text dt :discovery/pid-alive? pid :discovery/port-open? port
           :hive-mcp/processes procs :dirge/config-text ct)))

(deftest report-invariants-over-every-fact-combination
  (doseq [facts all-facts
          :let [{:keys [ok? checks] :as r} (d/report facts)]]
    (is (= [1 2] (map :check/step checks)))
    (is (= ok? (every? #(= :pass (status %)) checks)))
    (is (every? #(contains? #{:pass :fail} (status %)) checks))
    (is (every? #(= (= :fail (status %)) (some? (:check/fix %))) checks))
    (is (string? (d/render r)))))

(deftest report-ok-only-when-both-steps-pass
  (is (:ok? (d/report healthy)))
  (is (not (:ok? (d/report (assoc healthy :discovery/text nil)))))
  (is (not (:ok? (d/report (assoc healthy :dirge/config-text nil))))))

(deftest render-marks-each-step
  (let [text (d/render (d/report (assoc healthy :dirge/config-text nil)))]
    (is (str/includes? text "[ok] step 1"))
    (is (str/includes? text "[FAIL] step 2"))
    (is (str/includes? text "fix: add"))
    (is (str/ends-with? text "setup incomplete"))))
