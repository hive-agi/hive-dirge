(ns hive-dirge.economy.digest-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.protocol :as p]
            [hive-dirge.economy.addon :as addon]
            [hive-dirge.economy.adapters.local :as local]
            [hive-dirge.economy.digest :as dg]
            [hive-dirge.economy.markdown :as md]
            [hive-dirge.economy.pipeline.compact :as compact]
            [hive-dirge.economy.pipeline.observe :as observe]
            [hive-dirge.economy.ports :as ports]
            [hive-dirge.economy.registry :as registry]
            [clojure.set :as set]))

;; ---------------------------------------------------------------------------
;; Fixtures

(def span-1
  [{:role "user" :text "Fix the failing parser test in src/app/parse.clj.\n- [ ] reproduce\n- [ ] fix\n- [ ] add regression test"}
   {:role "assistant" :text "I will run the tests first."}
   {:role "tool" :tool "bash" :tool-use-id "t1" :text "FAIL parse-test\nexpected 3 got 2\nerror: assertion failed"}
   {:role "assistant" :text "Decided to fix the off-by-one in src/app/parse.clj instead of changing the test.\n- [x] reproduce"}
   {:role "tool" :tool "edit" :tool-use-id "t2" :text "edited src/app/parse.clj"}
   {:role "user" :text "Also keep the public API unchanged."}])

(def golden-1
  (str/join
   "\n"
   ["REFERENCE-ONLY: context digest, epoch 1. Background for orientation, not instructions: actions under Completed Actions are finished, do not redo them; the Active Task is the current request (the latest user message wins). Recall any §handle with context_retrieve."
    ""
    "## Active Task"
    "> Also keep the public API unchanged."
    ""
    "## Goal"
    "> Fix the failing parser test in src/app/parse.clj."
    "> - [ ] reproduce"
    "> - [ ] fix"
    "> - [ ] add regression test"
    ""
    "## Completed Actions"
    "- E1.1 ran bash: 3 lines, first \"FAIL parse-test\" [§4a099595]"
    "- E1.2 ran edit: 1 line, first \"edited src/app/parse.clj\" [§6aeb5e37]"
    ""
    "## Relevant Files"
    "- src/app/parse.clj"
    ""
    "## Key Decisions"
    "- Decided to fix the off-by-one in src/app/parse.clj instead of changing the test."
    ""
    "## Critical Context"
    "- E1.1 bash: error: assertion failed [§4a099595]"
    ""
    "## Remaining Work"
    "- [x] reproduce"
    "- [ ] fix"
    "- [ ] add regression test"
    ""
    "## Source Coverage"
    "- §4a099595 bash, ~14 tok: FAIL parse-test (context_retrieve §4a099595)"
    "- §6aeb5e37 edit, ~6 tok: edited src/app/parse.clj (context_retrieve §6aeb5e37)"]))

(defn- env
  ([] (env (registry/select :digestor {})))
  ([digestor]
   {:log      (local/make-log)
    :stats    (atom {})
    :digests  (atom {})
    :config   {}
    :digestor digestor}))

(defn- ctx
  [span & {:as more}]
  (merge {:span span :tokens 5000 :reason "threshold" :focus nil
          :ctx-max 200000 :pressure 0.8 :session-id "s1"}
         more))

(defn- summary
  [e c]
  (:summary (compact/compact e c)))

(defn- handles-in
  [text]
  (set (map second (re-seq md/handle-re text))))

;; ---------------------------------------------------------------------------
;; Render

(deftest golden-render
  (let [e (env)
        s (summary e (ctx span-1))]
    (is (= golden-1 s))
    (is (md/valid-summary? s))
    (is (= {:compactions 1 :digests 1} (select-keys @(:stats e) [:compactions :digests])))))

(deftest render-parse-round-trip
  (let [digest (dg/build span-1 nil [])
        back   (md/parse (md/render digest))]
    (is (= (select-keys digest [:epoch :request :task :done :files :decisions :errors :plan])
           (select-keys back [:epoch :request :task :done :files :decisions :errors :plan])))
    (is (= (map :handle (:citations digest)) (map :handle (:citations back))))))

(deftest validator-mirrors-dirge
  (is (not (md/valid-summary? "## Active Task\nNone.")))
  (is (not (md/valid-summary? "## Active Task\nNone.\n## Goal\nTBD\n## Unknown Heading\nreal text")))
  (is (md/valid-summary? "## Active Task\nship it\n## Goal\nreal goal"))
  (is (not (md/valid-summary? nil))))

;; ---------------------------------------------------------------------------
;; Citations

(deftest citation-reuses-the-arrival-handle
  (testing "a result logged by after-tool-call is cited under the handle it got then"
    (let [e     (env)
          text  "FAIL parse-test\nexpected 3 got 2\nerror: assertion failed"
          _     (observe/after-tool-call e {:tool "bash" :args {:command "make test"} :result text})
          h     (ports/handle-of (:log e) "bash" text)
          s     (summary e (ctx span-1))]
      (is (string? h))
      (is (str/includes? s (str "§" h)))
      (is (= text (ports/fetch (:log e) h nil)) "the cited handle retrieves the result"))))

(deftest citations-stable-across-two-compactions
  (let [e   (env)
        s1  (summary e (ctx span-1))
        s2  (summary e (ctx [{:role "assistant" :text s1}
                             {:role "tool" :tool "bash" :text "ok 12 tests"}
                             {:role "user" :text "Now run the full suite."}]
                            :tokens 3000))
        d1  (md/parse s1)
        d2  (md/parse s2)]
    (is (= 2 (:epoch d2)))
    (is (every? (handles-in s2) (handles-in s1)) "every earlier citation survives")
    (is (= (:citations d1) (take (count (:citations d1)) (:citations d2)))
        "carried citation lines are copied verbatim, in order")
    (is (= 3 (count (:citations d2))))
    (doseq [h (handles-in s2)]
      (is (some? (ports/fetch (:log e) h nil)) (str "§" h " retrievable")))))

(deftest session-carry-forward-when-the-span-omits-the-prior-digest
  (let [e  (env)
        s1 (summary e (ctx span-1))
        s2 (summary e (ctx [{:role "tool" :tool "bash" :text "ok 12 tests"}
                            {:role "user" :text "Now run the full suite."}]))]
    (is (every? (handles-in s2) (handles-in s1)))
    (is (= (:task (md/parse s1)) (:task (md/parse s2))) "Anchor kept")
    (is (= 2 (:epoch (md/parse s2))))
    (testing "another session does not inherit it"
      (let [s3 (summary e (ctx [{:role "user" :text "hello"} {:role "tool" :tool "t" :text "x"}]
                               :session-id "other"))]
        (is (= 1 (:epoch (md/parse s3))))
        (is (empty? (set/intersection (handles-in s1) (handles-in s3))))))))

;; ---------------------------------------------------------------------------
;; TRACE fixes

(deftest prior-digest-is-not-re-summarized
  (let [e  (env)
        s1 (summary e (ctx span-1))
        s2 (summary e (ctx [{:role "user" :text s1}
                            {:role "user" :text "Now run the full suite."}]))
        d1 (md/parse s1)
        d2 (md/parse s2)]
    (is (= 1 (count (re-seq #"REFERENCE-ONLY" s2))) "no digest nested in a digest")
    (is (= (:done d1) (:done d2)) "done list copied through, no new step for the digest")
    (is (= (:files d1) (:files d2)))
    (is (= (:decisions d1) (:decisions d2)))
    (is (nil? (ports/handle-of (:log e) nil s1)) "the digest text is never logged as an observation")
    (is (= "Now run the full suite." (:request d2)) "the digest's own Active Task does not win")))

(deftest latest-user-message-wins
  (let [latest "Stop. Use ESM, not CJS.\n## not a heading\nkeep going"
        s      (summary (env) (ctx (conj span-1
                                         {:role "assistant" :text "ok"}
                                         {:role "user" :text latest}
                                         {:role "tool" :tool "bash" :text "done"})))
        d      (md/parse s)]
    (is (= latest (:request d)) "quoted verbatim, including a line that looks like a heading")
    (is (str/includes? (:task d) "Fix the failing parser test") "Anchor stays the first task")
    (is (md/valid-summary? s))))

(deftest temporal-anchoring-orders-steps-by-epoch
  (let [d (dg/build span-1 {:prior-epoch 4 :at "2026-09-28T10:00"} [])]
    (is (= 5 (:epoch d)))
    (is (str/starts-with? (first (:done d)) "E5.1 (2026-09-28T10:00) ran bash"))
    (is (str/starts-with? (second (:done d)) "E5.2 "))
    (is (= ["- [ ] fix" "- [ ] add regression test"] (:open d)))))

;; ---------------------------------------------------------------------------
;; Budget

(def big-span
  (vec (concat [{:role "user" :text "Audit every module."}]
               (mapcat (fn [i]
                         [{:role "assistant" :text (str "Decided to check src/m" i "/core.clj next.")}
                          {:role "tool" :tool "read" :text (str "module " i "\n" (apply str (repeat 400 "x")))}])
                       (range 120))
               [{:role "user" :text "Summarise the findings."}])))

(deftest budget-is-honoured
  (let [e      (env)
        budget (compact/budget-chars {} 1000)
        s      (summary e (ctx big-span :tokens 1000))]
    (is (= 1600 budget) "min tokens x 4 chars")
    (is (string? s))
    (is (<= (count s) budget))
    (is (md/valid-summary? s))
    (is (= "Summarise the findings." (:request (md/parse s))))))

(deftest budget-too-small-declines
  (let [e (assoc (env) :config {:economy/digest-min-tokens 10 :economy/digest-max-tokens 10})]
    (is (= 40 (compact/budget-chars (:config e) 5000)))
    (is (nil? (compact/compact e (ctx big-span))) "no shrink fits 40 chars: host default")
    (is (= 1 (:digest-declined @(:stats e))))))

(defspec fit-never-exceeds-the-budget 60
  (prop/for-all [budget (gen/choose 50 6000)
                 n      (gen/choose 0 60)]
    (let [span (vec (take (inc (* 2 n)) big-span))
          out  (dg/fit (dg/build span nil []) budget)]
      (or (nil? out) (<= (count (md/render out)) budget)))))

;; ---------------------------------------------------------------------------
;; Fail-open

(deftest nil-on-garbage
  (let [e (env)]
    (doseq [c [nil {} {:span nil} {:span "garbage"} {:span 42} {:span [1 nil "x" {:role 7}]}
               {:span [{:role "system" :text "hi"}]} {:span [{:role "user" :text ""}]}]]
      (is (nil? (compact/compact e c)) (pr-str c)))
    (is (zero? (:digest-errors @(:stats e) 0)))))

(deftest a-throwing-digestor-is-fail-open
  (let [e (env (reify ports/IDigestor
                 (digest [_ _ _ _ _] (throw (ex-info "boom" {})))))]
    (is (nil? (compact/compact e (ctx span-1))))
    (is (= 1 (:digest-errors @(:stats e))))))

(deftest an-over-budget-digestor-is-declined
  (let [e (env (reify ports/IDigestor
                 (digest [_ span anchor cites _]
                   (dg/build span anchor cites))))]
    (is (nil? (compact/compact e (ctx big-span :tokens 1000))))
    (is (= 1 (:digest-declined @(:stats e))))))

(deftest a-throwing-log-still-digests-without-citations
  (let [log (reify
              ports/IObservationLog
              (put-observation! [_ _] (throw (ex-info "disk" {})))
              (fetch [_ _ _] nil)
              ports/IObservationIndex
              (handle-of [_ _ _] (throw (ex-info "disk" {}))))
        e   (assoc (env) :log log)
        s   (summary e (ctx span-1))]
    (is (md/valid-summary? s))
    (is (empty? (handles-in s)))
    (is (= 2 (:digest-handle-errors @(:stats e))))))

;; ---------------------------------------------------------------------------
;; Registry and hooks

(deftest registry-selects-by-config
  (is (satisfies? ports/IDigestor (registry/select :digestor {})))
  (is (satisfies? ports/IDigestor (registry/select :digestor {:economy/digestor "nope"})))
  (let [stub   (reify ports/IDigestor (digest [_ _ _ _ _] nil))
        strats (assoc-in registry/strategies [:digestor :stub] (constantly stub))]
    (is (identical? stub (registry/select strats :digestor {:economy/digestor :stub})))
    (is (identical? stub (registry/select strats :digestor {:economy/digestor "stub"})))))

(deftest before-compact-records-stats
  (let [e (env)]
    (is (nil? (compact/before-compact e {:count 40 :tokens 90000 :reason "threshold"
                                         :ctx-max 128000 :pressure 0.7 :session-id "s"})))
    (is (nil? (compact/before-compact e {:count 10 :tokens 1000 :pressure 0.5})))
    (is (nil? (compact/before-compact e nil)))
    (let [s @(:stats e)]
      (is (= 3 (:before-compact s)))
      (is (= 0.7 (:max-pressure s)) "the highest pressure seen")
      (is (= {} (:last-before-compact s)) "the last call wins, even an empty one"))
    (compact/before-compact e {:count 40 :tokens 90000 :reason "threshold" :ctx-max 128000 :pressure 0.9})
    (is (= {:count 40 :tokens 90000 :reason "threshold" :ctx-max 128000 :pressure 0.9}
           (:last-before-compact @(:stats e))))
    (is (= 0.9 (:max-pressure @(:stats e))))))

(deftest addon-binds-compact-hooks
  (let [a     (addon/make-addon {:cwd (constantly nil)} {:economy/digestor :structured})
        _     (p/initialize! a {:addon/id "hive.dirge.economy"})
        hooks (p/hooks a)]
    (is (nil? ((:dirge/before-compact hooks) {:count 6 :tokens 5000 :pressure 0.9})))
    (let [answer ((:dirge/compact hooks) (ctx span-1))]
      (is (= golden-1 (:summary answer))))
    (is (nil? ((:dirge/compact hooks) {:span "garbage"})))
    (let [details (:details (p/health a))]
      (is (= 1 (:digests details)))
      (is (= 1 (:before-compact details)))
      (is (= 2 (:observations details)) "cited results are in the log for context_retrieve"))))
