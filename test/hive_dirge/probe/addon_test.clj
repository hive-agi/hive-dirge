(ns hive-dirge.probe.addon-test
  "The probe addon against the hive-addon contract: manifest -> constructor
   -> IAddon -> lifecycle round-trip. Uses the real hive-addon mount boundary
   (parse-spec / resolve-constructor) as the port; no with-redefs."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.protocol :as p]
            [hive-dsl.result :as r]
            [hive-dirge.probe.addon :as probe]))

(def manifest-path "META-INF/hive-addons/hive-dirge-probe.edn")

(defn- manifest-spec []
  (let [res (io/resource manifest-path)]
    (is (some? res) "manifest is on the classpath")
    (boundary/parse-spec (slurp res))))

(deftest manifest-reads-and-validates
  (let [spec (manifest-spec)]
    (is (r/ok? spec) (pr-str spec))
    (let [m (:ok spec)]
      (is (= "hive.dirge.probe" (:addon/id m)))
      (is (= "hive-dirge.probe.addon" (:addon/init-ns m)))
      (is (= "addon-ctor" (:addon/init-fn m)))
      (is (= :foss (:addon/trust-class m))))))

(deftest manifest-is-discovered-on-classpath
  (let [{:keys [specs]} (boundary/discover-specs)]
    (is (some #(= "hive.dirge.probe" (:addon/id %)) specs))))

(deftest constructor-resolves
  (let [resolution (boundary/resolve-constructor (:ok (manifest-spec)))]
    (is (= :resolved (:constructor/status resolution)) (pr-str resolution))
    (is (= "hive-dirge.probe.addon/addon-ctor" (:constructor/symbol resolution)))
    (is (identical? probe/addon-ctor (:constructor resolution)))))

(deftest ctor-returns-an-iaddon
  (let [ctor  (:constructor (boundary/resolve-constructor (:ok (manifest-spec))))
        addon (ctor {})]
    (is (p/addon? addon))
    (is (satisfies? p/IAddon addon))
    (is (= "hive.dirge.probe" (p/addon-id addon)))
    (is (p/valid-addon-type? (p/addon-type addon)))
    (is (contains? (p/capabilities addon) :tools))
    (is (= #{} (p/excluded-tools addon)))
    (is (= {} (p/hooks addon)))))

(deftest lifecycle-round-trip
  (let [addon (probe/addon-ctor {})]
    (testing "before initialize! the addon reports down"
      (is (= :down (:status (p/health addon)))))
    (testing "initialize! succeeds and health reflects the config"
      (let [init (p/initialize! addon {:addon/id "hive.dirge.probe" :k 1})]
        (is (:success? init))
        (is (= [] (:errors init))))
      (let [h (p/health addon)]
        (is (= :ok (:status h)))
        (is (= 1 (get-in h [:details :config :k])))))
    (testing "the tool handler runs"
      (let [tool (first (p/tools addon))]
        (is (= "swarm-view" (:name tool)))
        (is (= "rows=100" (-> ((:handler tool) {:rows (vec (range 100))})
                              :content first :text)))))
    (testing "shutdown! succeeds and health goes back to down"
      (is (:success? (p/shutdown! addon)))
      (is (= :down (:status (p/health addon)))))
    (testing "re-initialize after shutdown works"
      (is (:success? (p/initialize! addon {})))
      (is (= :ok (:status (p/health addon)))))))

(deftest reload-fixture-v2-wins
  (let [v1 "test/fixtures/probe/addon.cljc"
        v2 "test/fixtures/probe/addon_v2.cljc"]
    (load-file v1)
    (is (= "hello-v1" ((resolve 'probe.addon/hello))))
    (load-file v2)
    (is (= "hello-v2" ((resolve 'probe.addon/hello))))
    (is (= "v2:2" ((resolve 'probe.addon/describe) ((resolve 'probe.addon/->Thing) 2))))
    (is (= :new-fn ((resolve 'probe.addon/added))))))
