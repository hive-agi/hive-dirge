(ns hive-dirge.doctor
  "Checks both /swarm setup steps on this machine and prints the report.

     clojure -M:doctor
     clojure -M:doctor --discovery PATH --dirge-config PATH

   Exit status 0 when both steps are done, 1 otherwise. Effects go through a
   ports map; `hive-dirge.doctor.domain` judges the gathered facts."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dirge.doctor.domain :as domain]
            [hive-dirge.host.domain :as host]))

;; SPDX-License-Identifier: MIT

(defn- read-file
  [path]
  (let [f (io/file path)]
    (when (.isFile f) (slurp f))))

(defn- pid-alive?
  [pid]
  (.isDirectory (io/file (str "/proc/" pid))))

(defn- port-open?
  [port]
  (try (with-open [s (java.net.Socket.)]
         (.connect s (java.net.InetSocketAddress. "127.0.0.1" (int port)) 500)
         true)
       (catch java.io.IOException _ false)))

(defn- cmdline
  [dir]
  (some-> (read-file (str dir "/cmdline")) (str/replace "\u0000" " ")))

(defn- hive-mcp-processes
  "Running JVMs whose command line names hive-mcp, other than this one."
  []
  (let [self (str (.pid (java.lang.ProcessHandle/current)))]
    (vec (for [^java.io.File dir (.listFiles (io/file "/proc"))
               :let [pid (.getName dir)]
               :when (and (re-matches #"\d+" pid) (not= pid self))
               :let [cmd (try (cmdline dir) (catch java.io.IOException _ nil))]
               :when (and cmd (str/includes? cmd "java") (str/includes? cmd "hive-mcp"))]
           {:pid (parse-long pid) :hive-dirge? (str/includes? cmd "hive-dirge")}))))

(def default-ports
  {:getenv     #(System/getenv ^String %)
   :read-file  read-file
   :pid-alive? pid-alive?
   :port-open? port-open?
   :processes  hive-mcp-processes})

(defn default-paths
  "Discovery file and dirge config paths from the environment in PORTS."
  [ports]
  {:discovery-path (host/discovery-path ((:getenv ports) "XDG_RUNTIME_DIR"))
   :config-path    (str ((:getenv ports) "HOME") "/.config/dirge/config.json")})

(defn gather-facts
  "The facts `domain/report` judges, read through PORTS at PATHS."
  [ports {:keys [discovery-path config-path]}]
  (let [text (:read-file ports)
        discovery (text discovery-path)
        {:keys [pid port]} (domain/discovery-endpoint discovery)]
    {:discovery/path discovery-path
     :discovery/text discovery
     :discovery/pid-alive? (when (integer? pid) ((:pid-alive? ports) pid))
     :discovery/port-open? (when (integer? port) ((:port-open? ports) port))
     :hive-mcp/processes (when-not discovery ((:processes ports)))
     :dirge/config-path config-path
     :dirge/config-text (text config-path)}))

(defn parse-args
  "--discovery PATH and --dirge-config PATH over DEFAULTS."
  [defaults args]
  (loop [acc defaults [flag v & more :as args] args]
    (case flag
      nil acc
      "--discovery" (recur (assoc acc :discovery-path v) more)
      "--dirge-config" (recur (assoc acc :config-path v) more)
      (throw (ex-info (str "unknown argument: " flag) {:args args})))))

(defn run
  "Gathers facts through PORTS for ARGS and answers the report."
  [ports args]
  (domain/report (gather-facts ports (parse-args (default-paths ports) args))))

(defn -main
  [& args]
  (let [report (run default-ports args)]
    (println (domain/render report))
    (shutdown-agents)
    (System/exit (if (:ok? report) 0 1))))
