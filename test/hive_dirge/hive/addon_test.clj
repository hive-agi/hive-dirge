(ns hive-dirge.hive.addon-test
  "hive.dirge against the hive-addon contract (manifest, constructor,
   lifecycle) and its command/tool pipeline over recording stub ports."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.protocol :as p]
            [hive-dsl.result :as r]
            [hive-dirge.hive.addon :as addon]))

(def manifest-path "META-INF/hive-addons/hive-dirge.edn")

(defn- manifest-spec []
  (let [res (io/resource manifest-path)]
    (is (some? res) "manifest is on the classpath")
    (boundary/parse-spec (slurp res))))

(defn- stub-ports
  "Ports recording every call into `calls`; `answers` maps an MCP tool name
   to the answer mcp-call returns."
  [calls answers]
  {:mcp-call   (fn [server tool args]
                 (swap! calls conj [:mcp server tool args])
                 (get answers tool {:error "no stub"}))
   :json-parse (fn [text]
                 (swap! calls conj [:json text])
                 (when (= text "ROWS")
                   [{:id "1" :title "t" :status "todo" :priority "high" :project "proj"}]))
   :panel!     (fn [op] (swap! calls conj [:panel op]) true)
   :cwd        (constantly "/w/proj")})

(defn- text-answer [s] {:content [{:type "text" :text s}] :isError false})

(defn- started [ports config]
  (doto (addon/make-addon ports) (p/initialize! config)))

(defn- hive-command [a]
  (get-in (p/hooks a) [:dirge/commands "hive" :handler]))

(deftest manifest-reads-and-resolves
  (let [spec (manifest-spec)]
    (is (r/ok? spec) (pr-str spec))
    (let [m (:ok spec)
          res (boundary/resolve-constructor m)]
      (is (= "hive.dirge" (:addon/id m)))
      (is (= "hive" (get-in m [:addon/config :hive/server])))
      (is (= :resolved (:constructor/status res)) (pr-str res))
      (is (identical? addon/addon-ctor (:constructor res)))))
  (is (some #(= "hive.dirge" (:addon/id %)) (:specs (boundary/discover-specs)))))

(deftest lifecycle-and-contract
  (let [a (addon/addon-ctor {})]
    (is (p/addon? a))
    (is (= "hive.dirge" (p/addon-id a)))
    (is (p/valid-addon-type? (p/addon-type a)))
    (is (= :down (:status (p/health a))))
    (is (:success? (p/initialize! a {:addon/id "hive.dirge" :addon/config {:hive/server "hv"}})))
    (is (= {:status :ok :details {:server "hv"}} (p/health a)))
    (is (= #{"hive_memory_search" "hive_kanban_list"} (set (map :name (p/tools a)))))
    (is (:success? (p/shutdown! a)))
    (is (= :down (:status (p/health a))))))

(deftest outside-dirge-is-a-no-op
  (testing "harness ports answer nil on the JVM, so commands report, never throw"
    (let [a (started addon/harness-ports {})]
      (is (str/includes? (:text ((hive-command a) {:args "catchup"})) "not running inside dirge"))
      (is (true? (:isError ((:handler (first (p/tools a))) {:query "x"})))))))

(deftest system-prompt-hook-makes-no-mcp-call
  (let [calls (atom [])
        a     (started (stub-ports calls {}) {:addon/config {:hive/server "hv"}})
        s     ((:dirge/system-prompt (p/hooks a)) {:cwd "/w" :session-id "s"})]
    (is (str/includes? s "/hive"))
    (is (= [] @calls))))

(deftest catchup-command
  (let [calls (atom [])
        a     (started (stub-ports calls {"project" (text-answer "CATCHUP")})
                       {:addon/config {:hive/server "hv"}})
        reply ((hive-command a) {:args "catchup" :argv ["catchup"] :cwd "/w/proj"})]
    (is (= [[:mcp "hv" "project" {"command" "workflow catchup" "directory" "/w/proj"}]] @calls))
    (is (str/includes? (:prompt reply) "CATCHUP"))
    (is (string? (:text reply)))))

(deftest catchup-refused-surfaces-error
  (let [a     (started (stub-ports (atom []) {"project" {:error "mcp-call is unavailable"}}) {})
        reply ((hive-command a) {:argv ["catchup"]})]
    (is (nil? (:prompt reply)))
    (is (str/includes? (:text reply) "mcp-call is unavailable"))))

(deftest wrap-command-uses-port-cwd-when-ctx-has-none
  (let [calls (atom [])
        a     (started (stub-ports calls {"project" (text-answer "wrapped!")}) {})]
    (is (= {:text "wrapped!"} ((hive-command a) {:argv ["wrap"]})))
    (is (= [[:mcp "hive" "project" {"command" "session wrap" "directory" "/w/proj"}]] @calls))))

(deftest kanban-command-shows-panel
  (let [calls (atom [])
        a     (started (stub-ports calls {"project" (text-answer "ROWS")}) {})
        reply ((hive-command a) {:argv ["kanban" "todo"] :cwd "/w/proj"})
        panel (some (fn [[k op]] (when (= k :panel) op)) @calls)]
    (is (= "todo" (get-in (first @calls) [3 "status"])))
    (is (= "hive kanban: 1 tasks, 1 in proj (side panel)" (:text reply)))
    (is (= :show (:op panel)))
    (is (= ["[todo] t  1"] (map :text (:lines panel))))))

(deftest kanban-unparsable-falls-back-to-markdown
  (let [calls (atom [])
        a     (started (stub-ports calls {"project" (text-answer "# not json")}) {})]
    ((hive-command a) {:argv ["kanban"]})
    (is (= "# not json" (:markdown (some (fn [[k op]] (when (= k :panel) op)) @calls))))))

(deftest help-and-unknown
  (let [calls (atom [])
        a     (started (stub-ports calls {}) {})]
    (is (str/includes? (:text ((hive-command a) {:args ""})) "/hive catchup"))
    (is (str/includes? (:text ((hive-command a) {:args "nope"})) "unknown /hive subcommand"))
    (is (= [] @calls))))

(deftest tools-proxy-hive
  (let [calls (atom [])
        a     (started (stub-ports calls {"memory"  (text-answer "M")
                                          "project" (text-answer "K")})
                       {:addon/config {:hive/server "hv" :hive/memory-limit 4}})
        by    (into {} (map (juxt :name :handler)) (p/tools a))]
    (is (= {:content [{:type "text" :text "M"}] :isError false}
           ((by "hive_memory_search") {:query "seal"})))
    (is (= {:content [{:type "text" :text "K"}] :isError false}
           ((by "hive_kanban_list") {:status "done"})))
    (is (= [[:mcp "hv" "memory" {"command" "search" "query" "seal" "limit" 4 "directory" "/w/proj"}]
            [:mcp "hv" "project" {"command" "kanban list" "limit" 50 "directory" "/w/proj" "status" "done"}]]
           @calls))))

(defn- parsing-ports
  "Stub ports whose json-parse answers `parses` by exact text."
  [calls answers parses]
  (assoc (stub-ports calls answers)
         :json-parse (fn [text] (swap! calls conj [:json text]) (get parses text))))

(deftest kanban-parses-past-appended-context-blocks
  (let [calls (atom [])
        a     (started (stub-ports calls {"project" (text-answer "ROWS\n\n---MEMORY---\n{:batch []}\n---/MEMORY---")}) {})
        reply ((hive-command a) {:argv ["kanban"] :cwd "/w/proj"})]
    (is (some #{[:json "ROWS"]} @calls) "the JSON reader never sees the context block")
    (is (= "hive kanban: 1 tasks, 1 in proj (side panel)" (:text reply)))))

(deftest memory-command-lists-hits
  (let [calls (atom [])
        a     (started (parsing-ports calls
                                      {"memory" (text-answer "HITS\n\n---MEMORY---\nx\n---/MEMORY---")}
                                      {"HITS" {:results [{:id "m1" :type "decision" :title "Reload"}]}})
                       {:addon/config {:hive/server "hv"}})
        reply ((hive-command a) {:argv ["memory" "addon" "reload"] :cwd "/w/proj"})]
    (is (= [:mcp "hv" "memory" {"command" "search" "query" "addon reload" "limit" 10 "directory" "/w/proj"}]
           (first @calls)))
    (is (= "1 memories for \"addon reload\"\n  [decision] Reload  (m1)" (:text reply))))
  (let [a (started (stub-ports (atom []) {}) {})]
    (is (str/includes? (:text ((hive-command a) {:argv ["memory"]})) "usage: /hive memory"))))

(deftest swarm-command-spans-every-project
  (let [calls (atom [])
        a     (started (parsing-ports calls
                                      {"swarm" (text-answer "AGENTS")}
                                      {"AGENTS" {:agents [{:id "b" :status "idle"}
                                                          {:id "a" :status "working" :project-id "dirge"}]}})
                       {})
        reply ((hive-command a) {:argv ["swarm"] :cwd "/w/proj"})
        panel (some (fn [[k op]] (when (= k :panel) op)) @calls)]
    (is (= [:mcp "hive" "swarm" {"command" "agent status" "verbosity" "slim"}] (first @calls))
        "no directory: the swarm is not one project's")
    (is (= "hive-swarm" (:id panel)))
    (is (= ["a working dirge" "b idle"] (map :text (:lines panel))))
    (is (= "hive swarm: 2 agent(s), 1 idle, 1 working (side panel)" (:text reply)))))

(deftest shout-command-posts-progress
  (let [calls (atom [])
        a     (started (stub-ports calls {"swarm" (text-answer "ok")}) {})]
    (is (= {:text "shouted to the hivemind"} ((hive-command a) {:argv ["shout" "s7" "merged"]})))
    (is (= [:mcp "hive" "swarm" {"command" "hivemind shout" "event_type" "progress"
                                  "task" "dirge" "message" "s7 merged" "directory" "/w/proj"}]
           (first @calls))))
  (let [a (started (stub-ports (atom []) {"swarm" {:error "mcp-call is unavailable"}}) {})]
    (is (str/includes? (:text ((hive-command a) {:argv ["shout" "x"]})) "mcp-call is unavailable"))))
