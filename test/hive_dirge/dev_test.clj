(ns hive-dirge.dev-test
  "REPL-friendliness: after one initialize!, redefining a var or adding an
   extra through hive-dirge.live changes what the addon answers. The subject
   here IS var rebinding, so the tests rebind vars and restore them."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.protocol :as p]
            [hive-dirge.dev :as dev]
            [hive-dirge.economy.addon :as econ]
            [hive-dirge.hive.addon :as hive]
            [hive-dirge.hive.domain :as hd]
            [hive-dirge.live :as live]))

(use-fixtures :each
  (fn [f]
    (let [addons @live/!addons
          extras @live/!extras]
      (try (f)
           (finally
             (reset! live/!addons addons)
             (reset! live/!extras extras))))))

(defn- rebound
  "Runs `f` with `v`'s root replaced by (g old-root), restoring it after."
  [v g f]
  (let [old @v]
    (alter-var-root v g)
    (try (f) (finally (alter-var-root v (constantly old))))))

(defn- started-economy []
  (doto (econ/make-addon {:cwd (constantly nil)}) (p/initialize! {})))

(deftest redefining-hook-map-changes-the-hooks-without-reinitialize
  (let [a       (started-economy)
        session (:session-id @(:state a))]
    (is (contains? (p/hooks a) :dirge/event))
    (rebound #'econ/hook-map
             (fn [orig] (fn [this] (assoc (orig this) :dirge/on-prompt (constantly "v2"))))
             (fn []
               (is (= "v2" ((:dirge/on-prompt (p/hooks a)) {})))
               (is (contains? (p/hooks a) :dirge/event))))
    (is (not (contains? (p/hooks a) :dirge/on-prompt)) "restored root, restored answer")
    (is (= session (:session-id @(:state a))) "no second initialize! ran")))

(deftest redefining-a-handler-var-reaches-an-issued-hook
  (let [a    (doto (hive/make-addon {:cwd (constantly "/w")}) (p/initialize! {}))
        hook (:dirge/system-prompt (p/hooks a))
        base (hook {})]
    (rebound #'hd/system-prompt (fn [_] (constantly "redefined"))
             #(is (= "redefined" (hook {}))))
    (is (= base (hook {})))))

(deftest addon-ctor-reads-harness-ports-at-call-time
  (let [calls (atom [])
        a     (doto (hive/addon-ctor {}) (p/initialize! {}))
        stub  {:mcp-call   (fn [server tool args]
                             (swap! calls conj [server tool args])
                             {:content [{:type "text" :text "[]"}] :isError false})
               :json-parse (constantly nil)
               :panel!     (constantly true)
               :cwd        (constantly "/w/proj")}
        run   #((:handler (first (p/tools a))) {:query "q"})]
    (is (true? (:isError (run))) "harness ports answer nil on the JVM")
    (rebound #'hive/harness-ports (constantly stub)
             #(is (false? (:isError (run)))))
    (is (= 1 (count @calls)))
    (is (= "/w/proj" (get-in @calls [0 2 "directory"])))))

(deftest live-extras-reach-hooks-and-tools-without-reinitialize
  (let [a        (started-economy)
        replaced {:name "context_retrieve" :description "stub" :inputSchema {} :handler (constantly :stub)}
        added    {:name "probe_tool" :description "p" :inputSchema {} :handler (constantly :probe)}]
    (live/add-hook! econ/addon-id-str :dirge/on-prompt (constantly "extra"))
    (live/add-tool! econ/addon-id-str replaced)
    (live/add-tool! econ/addon-id-str added)
    (is (= "extra" ((:dirge/on-prompt (p/hooks a)) {})))
    (is (= ["context_retrieve" "probe_tool"] (mapv :name (p/tools a))))
    (is (= :stub ((:handler (first (p/tools a))) {})) "an extra replaces the base tool of its name")
    (live/remove-tool! econ/addon-id-str "context_retrieve")
    (live/remove-hook! econ/addon-id-str :dirge/on-prompt)
    (is (not (contains? (p/hooks a) :dirge/on-prompt)))
    (is (not= :stub ((:handler (first (p/tools a))) {:handle "x"})))))

(deftest dev-inspects-the-live-addons
  (let [a (econ/addon-ctor {})]
    (p/initialize! a {})
    (is (false? (dev/refresh!)) "no dirge.harness on the JVM")
    (is (some #{econ/addon-id-str} (dev/ids)))
    (let [view (dev/inspect econ/addon-id-str)]
      (is (= ["context_retrieve"] (:tools view)))
      (is (some #{":dirge/event"} (:hooks view)))
      (is (not-any? #(str/includes? % "after-tool-call") (:hooks view)))
      (is (= :ok (get-in view [:health :status]))))
    (testing "add-hook! lands in the addon's hooks and in the view"
      (is (false? (dev/add-hook! econ/addon-id-str :dirge/on-prompt (constantly "hi"))))
      (is (= "hi" ((:dirge/on-prompt (p/hooks a)) {})))
      (is (= [":dirge/on-prompt"] (get-in (dev/inspect econ/addon-id-str) [:extras :hooks])))
      (dev/reset-extras! econ/addon-id-str)
      (is (= [] (get-in (dev/inspect econ/addon-id-str) [:extras :hooks]))))
    (is (nil? (dev/inspect "no.such.addon")))
    (is (contains? (dev/inspect) econ/addon-id-str))))
