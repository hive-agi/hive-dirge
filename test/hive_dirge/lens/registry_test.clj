(ns hive-dirge.lens.registry-test
  "The lens registry as pure data: registration, lookup, panel ops,
   invoke routing and capabilities derivation. No ports are exercised
   here beyond trivial stubs; the registry itself never performs effects."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dirge.lens.registry :as lens]
            [hive-dirge.hive.addon]
            [hive-addon.protocol]))

(def sample-a
  {:lens/id    :tasks
   :lens/title "Tasks"
   :lens/panel "tasks"
   :lens/keys  {"enter" {"invoke" "open"}}
   :lens/verbs {"open" (fn [_ports row _payload] [:opened row])}})

(def sample-b
  {:lens/id    :agents
   :lens/title "Agents"
   :lens/panel "agents"
   :lens/keys  {"x" "refresh"}
   :lens/verbs {"focus" (fn [_ports row _payload] [:focused row])}})

(def registry (lens/make-registry [sample-a sample-b]))

(deftest make-registry-and-lookup
  (is (= #{:tasks :agents} (set (keys (registry)))))
  (is (= sample-a (lens/lookup registry :tasks)))
  (is (= sample-a (lens/lookup registry "tasks")) "strings are accepted")
  (is (nil? (lens/lookup registry :missing)))
  (is (nil? (lens/lookup registry 42)))
  (is (= [sample-b sample-a] (lens/list-lenses registry)) "sorted by :lens/id"))

(deftest register-and-overlay
  (let [extra   {:lens/id :tasks :lens/title "Other"}
        merged  (lens/with-builtins registry [extra])]
    (is (= extra (lens/lookup merged :tasks)) "extra wins on :lens/id")
    (is (= sample-b (lens/lookup merged :agents)) "builtins survive the overlay"))
  (let [r (lens/register-lens registry {:lens/id :agents :lens/title "X"})]
    (is (= "X" (:lens/title (lens/lookup r :agents))))
    (is (= 2 (count (lens/list-lenses r))) "re-registration replaces, never duplicates"))
  (is (thrown? AssertionError (lens/register-lens registry {:lens/title "no id"}))))

(deftest parse-open
  (is (= :lens/list (:command (lens/parse-open registry nil))))
  (let [open (lens/parse-open registry "tasks")]
    (is (= :lens/open (:command open)))
    (is (= sample-a (:lens open))))
  (let [bad (lens/parse-open registry "nope")]
    (is (= :lens/unknown (:command bad)))
    (is (= [:agents :tasks] (:available bad))))
  (is (string? (lens/list-usage registry)))
  (is (re-find #"hive lenses:" (lens/list-usage registry))))

(deftest panel-op-carries-keys-and-cursor
  (let [op (lens/panel-op sample-a {:doc/title "Tasks" :doc/blocks []})]
    (is (= :ui/show-panel (:op op)))
    (is (= "tasks" (:panel/id op)))
    (is (= {:keys {"enter" {"invoke" "open"}}, :cursor false}
           (select-keys op [:keys :cursor]))))
  (let [op (lens/panel-op (assoc sample-a :lens/cursor? true) {})]
    (is (= {:keys {"enter" {"invoke" "open"}}, :cursor true}
           (select-keys op [:keys :cursor]))))
  (let [op (lens/panel-op sample-b {})]
    (is (= {:keys {"x" "refresh"}, :cursor false} (select-keys op [:keys :cursor]))))
  (testing "a lens without chords carries no keys field"
    (is (nil? (:keys (lens/panel-op {:lens/id :plain :lens/panel "p"} {}))))))

(deftest route-invoke
  (testing "a known panel and verb routes to the owning lens"
    (let [r (lens/route-invoke registry {:panel "tasks" :verb "open" :row "t1"})]
      (is (= :tasks (:invoke/routed r)))
      (is (= "open" (:verb r)))
      (is (= "t1" (:row r)))
      (is (= [:opened "t1"] ((:effects r) {})))))
  (testing "row may be nil (no cursor)"
    (let [r (lens/route-invoke registry {:panel "agents" :verb "focus" :row nil})]
      (is (= [:focused nil] ((:effects r) {})))))
  (testing "unknown verb on a known panel warns and ignores"
    (let [r (lens/route-invoke registry {:panel "tasks" :verb "nope"})]
      (is (nil? (:invoke/routed r)))
      (is (= "nope" (:verb r)))
      (is (= :tasks (:panel-owner r)))))
  (testing "unknown panel warns and ignores"
    (let [r (lens/route-invoke registry {:panel "ghost" :verb "open"})]
      (is (nil? (:invoke/routed r)))
      (is (= "ghost" (:panel r)))))
  (testing "payload defaults to {}"
    (let [verbed (fn [ports row payload] [ports row payload])
          reg    (lens/make-registry [(assoc sample-a :lens/verbs {"go" verbed})])
          r      (lens/route-invoke reg {:panel "tasks" :verb "go" :row "r"})]
      (is (= [{} "r" {}] ((:effects r) {}))))))

(deftest capabilities-fragment
  (let [caps (lens/capabilities-fragment registry)]
    (is (= ["focus" "open"] (get caps "invokes")) "every lens verb, sorted")
    (is (= {"enter" {"invoke" "open"}, "x" "refresh"}
           (get caps "keys")) "every lens chord merged")
    (is (every? string? (get caps "invokes"))))
  (testing "the builtin registry derives kanban and swarm chords"
    (let [caps (lens/capabilities-fragment (lens/builtin-registry))]
      (is (= ["focus" "open"] (get caps "invokes")))
      (is (= {"enter" {"invoke" "open"}}
             (get caps "keys")))))
  (testing "an empty registry answers empty fragment"
    (is (= {"invokes" [] "keys" {}} (lens/capabilities-fragment (lens/make-registry []))))))

(deftest owner-of
  (is (= sample-a (lens/owner-of registry "tasks")))
  (is (= sample-b (lens/owner-of registry "agents")))
  (is (nil? (lens/owner-of registry "ghost"))))

(deftest known-verb
  (is (true? (lens/known-verb? sample-a "open")))
  (is (false? (lens/known-verb? sample-a "focus"))))


(deftest third-party-registration-hook
  (let [calls (atom [])
        ports {:panel! #(swap! calls conj %) :cwd (constantly "/w")}
        addon (hive-dirge.hive.addon/make-addon ports)
        extra (assoc sample-a :lens/open (fn [_ _ _]
                                           {:fx [(lens/panel-op sample-a
                                                               {:doc/title "Tasks" :doc/blocks []})]}) )
        hooks (hive-addon.protocol/hooks addon)]
    ((:dirge/register-lenses! hooks) [extra])
    (is (some #(= :tasks (:lens/id %)) ((:dirge/lenses hooks))))
    (is (= "lens :tasks opened"
           (:text ((get-in hooks [:dirge/commands "hive" :handler])
                   {:argv ["tasks"]}))))
    (is (= "tasks" (:panel/id (first @calls))))))
