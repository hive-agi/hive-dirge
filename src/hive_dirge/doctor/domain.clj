(ns hive-dirge.doctor.domain
  "Pure checks of the two /swarm setup steps. Facts in, report out.

   Facts:
     :discovery/path      discovery file path
     :discovery/text      its contents, nil when absent
     :discovery/pid-alive? true, false, or nil (no pid / unknown)
     :discovery/port-open? true, false, or nil (not probed)
     :hive-mcp/processes  [{:pid n :hive-dirge? bool}], nil when not scanned
     :dirge/config-path   dirge config.json path
     :dirge/config-text   its contents, nil when absent"
  (:require [clojure.string :as str]
            [hive-vessel.wire :as wire]))

;; SPDX-License-Identifier: MIT

(def expected-discovery-dir "hive-vessel")

(defn- parse-json
  "TEXT as JSON data, or {::parse-error message}."
  [text]
  (try (wire/read-json text)
       (catch Throwable t {::parse-error (or (ex-message t) (str t))})))

(defn- result
  [id step status summary fix]
  (cond-> {:check/id id :check/step step :check/status status :check/summary summary}
    fix (assoc :check/fix fix)))

(def ^:private step-1-fix
  (str "put hive-dirge on hive-mcp's classpath (local.deps.edn "
       "{:deps {io.github.hive-agi/hive-dirge {:local/root \"../hive-dirge\"}}}) "
       "and restart it, or mount it live with hive's hot tool: "
       "inject path=/path/to/hive-dirge resolve_deps=false"))

(defn- classpath-hint
  [processes]
  (cond
    (nil? processes) nil
    (empty? processes) "no running hive-mcp process found"
    (some :hive-dirge? processes) (str "a hive-mcp process names hive-dirge on its classpath "
                                       "but hive.dirge.host did not write the file; check its log")
    :else "running hive-mcp processes do not name hive-dirge on their classpath"))

(defn discovery-endpoint
  "{:pid :port} named by discovery TEXT, nil when it is not a JSON object."
  [text]
  (let [doc (some-> text parse-json)]
    (when (and (map? doc) (not (::parse-error doc)))
      {:pid (get doc "pid") :port (get doc "port")})))

(defn check-host
  "Step 1: hive-mcp mounted hive.dirge.host, seen through its discovery file."
  [{:discovery/keys [path text pid-alive? port-open?] :hive-mcp/keys [processes]}]
  (let [doc (some-> text parse-json)
        hint (classpath-hint processes)
        fail (fn [summary]
               (result :swarm/host 1 :fail
                       (cond-> summary hint (str "; " hint))
                       step-1-fix))]
    (cond
      (nil? text) (fail (str "no discovery file at " path))
      (::parse-error doc) (fail (str "discovery file " path " is not JSON: " (::parse-error doc)))
      (not (map? doc)) (fail (str "discovery file " path " is not a JSON object"))
      (not= "dirge" (get doc "vessel")) (fail (str "discovery file " path " is not for vessel dirge"))
      (false? pid-alive?) (fail (str "discovery file " path " is stale: pid "
                                     (get doc "pid") " is not running"))
      (false? port-open?) (fail (str "discovery file " path " is stale: nothing listens on port "
                                     (get doc "port")))
      :else (result :swarm/host 1 :pass
                    (str "hive.dirge.host is mounted: " path " -> " (get doc "url"))
                    nil))))

(def ^:private step-2-fix
  (str "add { \"panel_feed\": { \"discovery_dir\": \"" expected-discovery-dir "\" } } "
       "to dirge's config.json, then restart dirge"))

(defn check-dirge-config
  "Step 2: dirge's config subscribes to the feed with panel_feed.discovery_dir."
  [{:dirge/keys [config-path config-text]}]
  (let [doc (some-> config-text parse-json)
        feed (when (map? doc) (get doc "panel_feed"))
        dir (when (map? feed) (get feed "discovery_dir"))
        fail #(result :swarm/dirge-config 2 :fail % step-2-fix)]
    (cond
      (nil? config-text) (fail (str "no dirge config at " config-path))
      (::parse-error doc) (fail (str config-path " is not JSON: " (::parse-error doc)))
      (not (map? doc)) (fail (str config-path " is not a JSON object"))
      (nil? feed) (fail (str config-path " has no panel_feed"))
      (not= expected-discovery-dir dir)
      (fail (str config-path " panel_feed.discovery_dir is "
                 (if (nil? dir) "unset" (pr-str dir))
                 ", expected " (pr-str expected-discovery-dir)))
      :else (result :swarm/dirge-config 2 :pass
                    (str config-path " subscribes with panel_feed.discovery_dir "
                         (pr-str dir) " (dirge reads it at startup)")
                    nil))))

(defn report
  "Both checks over FACTS: {:ok? bool :checks [check-1 check-2]}."
  [facts]
  (let [checks [(check-host facts) (check-dirge-config facts)]]
    {:ok? (every? #(= :pass (:check/status %)) checks)
     :checks checks}))

(defn render
  "REPORT as terminal text."
  [{:keys [ok? checks]}]
  (str/join "\n"
            (concat
             ["hive-dirge doctor: /swarm setup"]
             (mapcat (fn [{:check/keys [step status summary fix]}]
                       (cond-> [(str "  [" (if (= :pass status) "ok" "FAIL") "] step " step ": " summary)]
                         fix (conj (str "         fix: " fix))))
                     checks)
             [(if ok? "all steps done" "setup incomplete")])))
