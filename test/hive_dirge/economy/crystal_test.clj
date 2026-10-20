(ns hive-dirge.economy.crystal-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-dirge.economy.adapters.hive-memory :as hm]
            [hive-dirge.economy.crystal :as crystal]
            [hive-dirge.economy.pipeline.crystallize :as cz]
            [hive-dirge.economy.ports.crystallizer :as p]
            [hive-test.trifecta :refer [deftrifecta]]))

;; ---------------------------------------------------------------------------
;; Pure: Digest -> memory entry

(defn- tagged-digest-entry?
  [e]
  (or (nil? e)
      (and (= "note" (:type e))
           (string? (:content e))
           (not (str/blank? (:content e)))
           (= crystal/tag (first (:tags e))))))

(defn- entry-of [{:keys [md meta]}] (crystal/entry md meta))

(def gen-input
  (gen/hash-map :md   (gen/one-of [(gen/return nil) (gen/return "") gen/string-alphanumeric])
                :meta (gen/hash-map :session-id (gen/one-of [(gen/return nil) gen/string-alphanumeric])
                                    :cwd        (gen/one-of [(gen/return nil) gen/string-alphanumeric])
                                    :epoch      (gen/one-of [(gen/return nil) gen/nat]))))

(deftrifecta digest-entry
  hive-dirge.economy.crystal-test/entry-of
  {:golden-path "test/golden/economy-crystal-entry.edn"
   :cases       {:full    {:md "## Digest\n- done" :meta {:session-id "s1" :cwd "/p/x" :epoch 2}}
                 :bare    {:md "## Digest" :meta {}}
                 :blank   {:md "  " :meta {:session-id "s1"}}}
   :gen         gen-input
   :pred        tagged-digest-entry?
   :num-tests   100
   :mutations   [["untagged" (fn [_] {:type "note" :content "x" :tags []})]
                 ["empty content" (fn [_] {:type "note" :content "" :tags [crystal/tag]})]]})

(deftest entry-names-session-epoch-and-directory
  (is (= {:type "note" :content "md" :duration "medium" :directory "/p/x"
          :tags ["economy-digest" "session:s1" "epoch:2"]}
         (crystal/entry "md" {:session-id "s1" :cwd "/p/x" :epoch 2}))))

(deftest texts-and-seed-context
  (is (= ["a" "b"] (crystal/texts {:results [{:content "a"} {:preview "b"} {:id "x"}]})))
  (is (= ["a"] (crystal/texts [{:content "a"}])))
  (is (= [] (crystal/texts "junk")))
  (is (nil? (crystal/seed-context [])))
  (let [s (crystal/seed-context ["d2" "d1" "d0"] 2 1000)]
    (is (str/includes? s "d2"))
    (is (str/includes? s "d1"))
    (is (not (str/includes? s "d0"))))
  (is (= 10 (count (crystal/seed-context [(apply str (repeat 50 "x"))] 3 10)))))

;; ---------------------------------------------------------------------------
;; Pipeline against a recording stub port

(defrecord Recording [filed seeded ok?]
  p/ICrystallizer
  (crystallize! [_ e] (swap! filed conj e) ok?)
  (seeds [_ _dir n] (vec (take n seeded))))

(defn- env [config stub]
  {:crystallizer stub :stats (atom {:last-digest {:epoch 3}}) :config config})

(deftest after-compact-files-the-digest-only-when-enabled
  (let [stub (->Recording (atom []) [] true)
        on   (env {:economy/crystallize? true} stub)
        ans  {:summary "## Digest"}]
    (is (= ans (cz/after-compact! on {:session-id "s1" :cwd "/p"} ans)))
    (is (= [{:type "note" :content "## Digest" :duration "medium" :directory "/p"
             :tags ["economy-digest" "session:s1" "epoch:3"]}]
           @(:filed stub)))
    (is (= 1 (:crystals @(:stats on))))
    (testing "nil answer files nothing"
      (cz/after-compact! on {} nil)
      (is (= 1 (count @(:filed stub)))))
    (testing "off by default"
      (cz/after-compact! (env {} stub) {} ans)
      (is (= 1 (count @(:filed stub)))))
    (testing "a refused write is counted, not thrown"
      (let [bad (env {:economy/crystallize? true} (->Recording (atom []) [] false))]
        (cz/after-compact! bad {} ans)
        (is (= 1 (:crystal-errors @(:stats bad))))))))

(deftest seed-answers-context-from-the-newest-digests
  (let [stub (->Recording (atom []) ["new" "old"] true)]
    (is (str/includes? (:context (cz/seed (env {:economy/crystallize? true} stub) "/p")) "new"))
    (is (nil? (cz/seed (env {} stub) "/p")))
    (is (nil? (cz/seed (env {:economy/crystallize? true} (->Recording (atom []) [] true)) "/p")))))

;; ---------------------------------------------------------------------------
;; Adapter against a stub mcp-call port

(deftest hive-memory-adapter-speaks-the-memory-tool
  (let [calls (atom [])
        call  (fn [s t a] (swap! calls conj [s t a])
                {:content [{:type "text" :text "[{\"content\":\"d1\"}]"}]})
        c     (hm/make-crystallizer {:mcp-call call :json-parse (fn [_] [{:content "d1"}])})]
    (is (true? (p/crystallize! c (crystal/entry "md" {:cwd "/p"}))))
    (is (= ["hive" "memory" "add"] [(ffirst @calls) (second (first @calls)) (get-in (first @calls) [2 "command"])]))
    (is (= ["d1"] (p/seeds c "/p" 3)))
    (is (= {"command" "query" "tags" ["economy-digest"] "limit" 3 "verbosity" "full" "directory" "/p"}
           (nth (second @calls) 2)))
    (is (nil? (hm/make-crystallizer {})))
    (testing "fail-open"
      (let [boom (hm/make-crystallizer {:mcp-call (fn [& _] (throw (ex-info "down" {})))})]
        (is (false? (p/crystallize! boom (crystal/entry "md" {}))))
        (is (= [] (p/seeds boom "/p" 3)))))))
