(ns hive-dirge.economy.mask-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-addon.protocol :as p]
            [hive-dirge.economy.addon :as addon]
            [hive-dirge.economy.adapters.local :as local]
            [hive-dirge.economy.domain :as d]
            [hive-dirge.economy.mask :as mask]
            [hive-dirge.economy.pipeline.shape :as shape]
            [hive-dirge.economy.ports :as ports]
            [hive-dirge.economy.registry :as registry]
            [hive-test.trifecta :refer [deftrifecta]]))

;; ---------------------------------------------------------------------------
;; Synthetic transcripts in dirge's message shape

(defn- big [s] (apply str s ": " (repeat 60 "line of output\n")))

(defn- turn
  "One assistant tool call and its result."
  [i]
  [{:role "assistant" :content [{:type "toolCall" :id (str "c" i) :name "bash"
                                 :arguments {:command (str "cmd " i)}}]}
   {:role "toolResult" :toolCallId (str "c" i) :toolName "bash"
    :content [{:type "text" :text (big (str "out" i))}] :isError false}])

(defn- transcript
  [n]
  (into [{:role "user" :content "fix the build"}] (mapcat turn) (range n)))

(defn- handles-for
  "{index handle} a fake log would answer: one per maskable result."
  [keep-turns ms]
  (into {} (map (fn [{:keys [index]}] [index (format "%08x" (+ 0xa0000000 index))]))
        (mask/maskable keep-turns ms)))

(defn- run-mask
  "[keep-turns messages] -> masked messages, with a handle for every result."
  [[k ms]]
  (mask/mask-messages {:keep-turns k :handles (handles-for k ms)} ms))

(def gen-case
  (gen/let [k (gen/choose 1 4)
            n (gen/choose 0 8)]
    [k (transcript n)]))

(defn- recent-results-whole?
  "No tool result in the last `k` turns is a stub."
  [[k ms] out]
  (let [ag (mask/ages ms)]
    (every? (fn [i] (or (>= (nth ag i) k) (= (nth ms i) (nth out i))))
            (range (count ms)))))

(defn- every-stub-carries-its-handle?
  [[k ms] out]
  (let [hs (handles-for k ms)]
    (every? (fn [[i h]] (str/includes? (mask/content-text (:content (nth out i))) (d/cite h)))
            hs)))

(defn- mask-invariants
  [[_ ms :as input]]
  (let [out (run-mask input)]
    (and (= (count ms) (count out))
         (= (map :role ms) (map :role out))
         (= (filter #(not= "toolResult" (:role %)) ms)
            (filter #(not= "toolResult" (:role %)) out))
         (recent-results-whole? input out)
         (every-stub-carries-its-handle? input out))))

;; Idempotence over the pair: masking a masked transcript changes nothing.
(defn- mask-pair [[k ms :as input]] [k (run-mask input)])

(deftrifecta mask-messages-idempotent
  hive-dirge.economy.mask-test/mask-pair
  {:gen         gen-case
   :idempotent? true
   :num-tests   100})

(deftrifecta mask-messages-invariants
  hive-dirge.economy.mask-test/mask-invariants
  {:gen       gen-case
   :pred      true?
   :num-tests 100
   :mutations [["drops-a-message" (fn [_] false)]]
   :assert    (fn []
                (is (true? (mask-invariants [1 (transcript 3)])))
                (is (true? (mask-invariants [2 (transcript 5)]))))})

(deftrifecta mask-messages-masks-old-results
  hive-dirge.economy.mask/mask-messages
  {:mutations [["masks-nothing" (fn [_ ms] ms)]
               ["masks-everything" (fn [{:keys [handles]} ms]
                                     (vec (map-indexed
                                           (fn [i m] (if (= "toolResult" (:role m))
                                                       (mask/stub-message m (mask/stub-text "bash" nil (get handles i "x")))
                                                       m))
                                           ms)))]]
   :assert    (fn []
                (let [ms  (transcript 3)
                      out (run-mask [1 ms])]
                  (testing "turns 0 and 1 are masked, the last turn is whole"
                    (is (mask/stub? (nth out 2)))
                    (is (mask/stub? (nth out 4)))
                    (is (= (nth ms 6) (nth out 6))))
                  (is (= "[masked: bash {:command \"cmd 0\"} §a0000002; context_retrieve recovers it]"
                         (mask/content-text (:content (nth out 2)))))))})

(deftest masking-is-off-by-default
  (is (nil? (mask/make-shaper {})))
  (is (nil? (mask/make-shaper {:economy/mask-after-turns 0})))
  (is (nil? (registry/select :shaper {})))
  (is (= [] (mask/maskable nil (transcript 4))))
  (is (some? (registry/select :shaper {:economy/mask-after-turns 2}))))

(deftest a-result-without-a-handle-stays-whole
  (let [ms (transcript 3)]
    (is (= ms (mask/mask-messages {:keep-turns 1 :handles {}} ms)))))

(deftest short-results-and-stubs-are-not-masked
  (let [ms [{:role "assistant" :content [{:type "toolCall" :id "c" :name "ls" :arguments {}}]}
            {:role "toolResult" :toolCallId "c" :toolName "ls" :content [{:type "text" :text "a b"}]}
            {:role "assistant" :content "done"}]]
    (is (= [] (mask/maskable 1 ms)))))

;; ---------------------------------------------------------------------------
;; Boundary pipeline over a stub log port

(defn- recording-log
  "IObservationLog stub that knows ids and records every write."
  [by-id]
  (let [puts (atom [])]
    {:puts puts
     :log  (reify
             ports/IObservationLog
             (put-observation! [_ o] (swap! puts conj o) "0000beef")
             (fetch [_ _ _] nil)
             ports/IObservationIndex
             (handle-of [_ _ _] nil)
             ports/IObservationJoin
             (handle-by-id [_ id] (get by-id id))
             (args-of [_ _] nil))}))

(deftest shape-joins-by-id-and-logs-the-rest
  (let [{:keys [log puts]} (recording-log {"c0" "1234abcd"})
        env {:log log :stats (atom {}) :shaper (mask/make-shaper {:economy/mask-after-turns 1})}
        out (:messages (shape/transform-context env {:messages (transcript 3)}))]
    (is (str/includes? (mask/content-text (:content (nth out 2))) "§1234abcd"))
    (is (str/includes? (mask/content-text (:content (nth out 4))) "§0000beef"))
    (is (= 1 (count @puts)) "only the result the log did not know is logged")
    (is (= {:shaped 1 :masked 2} @(:stats env)))))

(deftest shape-is-fail-open
  (let [env {:log (reify ports/IObservationLog
                    (put-observation! [_ _] (throw (ex-info "boom" {})))
                    (fetch [_ _ _] nil))
             :stats (atom {}) :shaper (mask/make-shaper {:economy/mask-after-turns 1})}]
    (is (nil? (shape/transform-context env {:messages (transcript 3)})) "no handle: no rewrite")
    (is (nil? (shape/transform-context (assoc env :shaper nil) {:messages (transcript 3)})))
    (is (nil? (shape/transform-context env {:messages "garbage"})))))

;; ---------------------------------------------------------------------------
;; Addon wiring with a stub port

(deftest addon-listens-only-when-masking-is-on
  (let [off (addon/make-addon {:cwd (constantly nil)})]
    (p/initialize! off {})
    (is (not (contains? (p/hooks off) :dirge/transform-context))))
  (let [on (addon/make-addon {:cwd (constantly nil)})
        _  (p/initialize! on {:addon/config {:economy/mask-after-turns 1}})
        hooks (p/hooks on)
        event (:dirge/event hooks)
        xform (:dirge/transform-context hooks)]
    (is (fn? xform))
    (event {:event :tool-call :id "c0" :tool "bash" :args {:command "cmd 0"}})
    (event {:event :tool-result :id "c0" :output (big "out0")})
    (let [out  (:messages (xform {:messages (transcript 2)}))
          stub (mask/content-text (:content (nth out 2)))
          h    (second (re-find #"§([0-9a-f]+)" stub))
          tool (:handler (first (p/tools on)))]
      (is (= 5 (count out)))
      (is (= (big "out0") (-> (tool {:handle (d/cite h)}) :content first :text))
          "the stub's handle retrieves the masked result")
      (is (= (nth (transcript 2) 4) (nth out 4)) "the last turn is whole"))))
