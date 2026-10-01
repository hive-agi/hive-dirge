(ns hive-dirge.doctor-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dirge.doctor :as doctor]
            [hive-dirge.doctor.domain-test :as fx]))

(defn- recording-ports
  "Stub ports over FILES (path -> text); CALLS records each probe."
  [files calls & {:keys [alive? open? processes] :or {alive? true open? true processes []}}]
  {:getenv     {"XDG_RUNTIME_DIR" "/run/user/1000" "HOME" "/h/u"}
   :read-file  files
   :pid-alive? (fn [pid] (swap! calls conj [:pid pid]) alive?)
   :port-open? (fn [port] (swap! calls conj [:port port]) open?)
   :processes  (fn [] (swap! calls conj [:processes]) processes)})

(deftest default-paths-follow-the-environment
  (is (= {:discovery-path fx/discovery-path :config-path fx/config-path}
         (doctor/default-paths (recording-ports {} (atom []))))))

(deftest gather-facts-probes-the-discovery-endpoint
  (let [calls (atom [])
        ports (recording-ports {fx/discovery-path fx/live-discovery
                                fx/config-path fx/good-config} calls)
        facts (doctor/gather-facts ports (doctor/default-paths ports))]
    (is (= [[:pid 42] [:port 4100]] @calls))
    (is (= fx/healthy (select-keys facts (keys fx/healthy))))
    (is (nil? (:hive-mcp/processes facts)))))

(deftest gather-facts-scans-processes-only-without-a-discovery-file
  (let [calls (atom [])
        ports (recording-ports {} calls :processes [{:pid 9 :hive-dirge? false}])
        facts (doctor/gather-facts ports (doctor/default-paths ports))]
    (is (= [[:processes]] @calls))
    (is (nil? (:discovery/text facts)))
    (is (nil? (:dirge/config-text facts)))
    (is (= [{:pid 9 :hive-dirge? false}] (:hive-mcp/processes facts)))))

(deftest run-reports-both-steps
  (testing "healthy machine"
    (is (:ok? (doctor/run (recording-ports {fx/discovery-path fx/live-discovery
                                            fx/config-path fx/good-config}
                                           (atom []))
                          []))))
  (testing "stale discovery file"
    (let [r (doctor/run (recording-ports {fx/discovery-path fx/live-discovery
                                          fx/config-path fx/good-config}
                                         (atom []) :alive? false)
                        [])]
      (is (not (:ok? r)))
      (is (= [:fail :pass] (map :check/status (:checks r))))))
  (testing "paths from arguments"
    (let [r (doctor/run (recording-ports {"/d.json" fx/live-discovery "/c.json" fx/good-config}
                                         (atom []))
                        ["--discovery" "/d.json" "--dirge-config" "/c.json"])]
      (is (:ok? r)))))

(deftest parse-args-rejects-unknown-flags
  (is (= {:discovery-path "a" :config-path "b"}
         (doctor/parse-args {} ["--discovery" "a" "--dirge-config" "b"])))
  (is (thrown? clojure.lang.ExceptionInfo (doctor/parse-args {} ["--nope"]))))
