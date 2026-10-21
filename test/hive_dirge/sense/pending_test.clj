(ns hive-dirge.sense.pending-test
  "Ops sent to dirge but not yet acked survive a restart of the relay.

   The relay drains sixth-sense consume-once, so an op held only in memory is
   lost when the hive JVM restarts between send and ack. With a pending store
   the held set is saved on every change and seeded back at start!, and the
   next (re)connection replays it. Nothing here is redefined: the store is an
   atom-backed IPendingStore, or the real EDN file adapter on a temp path."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-dirge.sense.domain :as domain]
            [hive-dirge.sense.pending-edn :as pending-edn]
            [hive-dirge.sense.ports :as ports]
            [hive-dirge.sense.relay :as relay]))

;; SPDX-License-Identifier: MIT

(defn- op [id] {"op" "loop/steer" "id" id "text" (str "text of " id)})

(defn- held [& ids] (reduce domain/hold domain/empty-pending (map op ids)))

;; =============================================================================
;; restore: whatever a store hands back becomes a well-formed pending set
;; =============================================================================

(defn- well-formed? [p]
  (let [{:keys [order ops]} p]
    (and (map? p)
         (vector? order)
         (map? ops)
         (= (count order) (count (distinct order)))
         (= (set order) (set (keys ops)))
         (<= (count order) domain/max-pending)
         (every? (fn [[k v]] (= k (get v "id"))) ops))))

(def ^:private gen-loaded
  (gen/one-of
   [(gen/return nil)
    (gen/return {})
    (gen/return [1 2])
    gen/string-alphanumeric
    (gen/fmap #(apply held %) (gen/vector (gen/elements ["a" "b" "c" "d"]) 0 6))
    (gen/fmap (fn [[order extra]]
                {:order (vec order) :ops (into {} (map (fn [i] [i (op i)])) extra)})
              (gen/tuple (gen/vector (gen/elements ["a" "b" "c"]) 0 5)
                         (gen/vector (gen/elements ["a" "b" "x"]) 0 4)))]))

(deftrifecta restore-contract
  hive-dirge.sense.domain/restore
  {:golden-path "test/hive_dirge/sense/golden/restore.edn"
   :cases       {:absent       nil
                 :garbage      "not a pending set"
                 :empty-map    {}
                 :held         (held "a" "b")
                 :orphan-order {:order ["a" "z"] :ops {"a" (op "a")}}
                 :orphan-op    {:order ["a"] :ops {"a" (op "a") "q" (op "q")}}
                 :dup-order    {:order ["a" "a"] :ops {"a" (op "a")}}
                 :wrong-id     {:order ["a"] :ops {"a" (op "b")}}}
   :gen         gen-loaded
   :pred        well-formed?
   :num-tests   200
   :mutations   [["trusts-the-file" identity]
                 ["forgets-everything" (constantly domain/empty-pending)]]})

;; =============================================================================
;; The relay across a restart
;; =============================================================================

(defrecord AtomStore [a]
  ports/IPendingStore
  (load-pending [_] @a)
  (save-pending! [_ pending] (reset! a pending) nil))

(defrecord QueueSource [senses]
  ports/ISenseSource
  (listen! [_ _ _] nil)
  (unlisten! [_ _] nil)
  (drain! [_ _ _] (first (reset-vals! senses []))))

(defn- sense [id]
  {:sense/id id :sense/class :sense/blocked :sense/agent "ling-1" :sense/text id})

(defn- relay-over
  "A relay whose first (asynchronous) pump finds no loop client, so every
   drain below happens on the test thread."
  [store senses sent client?]
  (let [r (relay/start! {:source        (->QueueSource senses)
                         :broadcast!    (fn [o] (swap! sent conj (get o "id")))
                         :loop-client?  (fn [] @client?)
                         :pending-store store})]
    (relay/stop! r)
    r))

(deftest ops-held-across-a-restart-are-replayed
  (let [store   (->AtomStore (atom nil))
        senses  (atom [(sense "s1") (sense "s2")])
        sent-1  (atom [])
        client? (atom false)
        r1      (relay-over store senses sent-1 client?)]
    (reset! client? true)
    (is (= 2 (relay/pump! r1)) "both senses are drained, sent and held")
    (is (= ["s1" "s2"] @sent-1))
    (is (= ["s1" "s2"] (:order @(:a store))) "the held set reached the store")
    (testing "a fresh relay over the same store (the hive restarted) replays them"
      (let [sent-2 (atom [])
            r2     (relay-over store (atom []) sent-2 (atom false))]
        (is (= 2 (:pending (relay/status r2))))
        (is (= 2 (relay/replay! r2)))
        (is (= ["s1" "s2"] @sent-2))
        (testing "an ack is saved too, so it is not replayed after the next restart"
          (is (true? (relay/ack! r2 "s1")))
          (let [sent-3 (atom [])
                r3     (relay-over store (atom []) sent-3 (atom false))]
            (is (= 1 (relay/replay! r3)))
            (is (= ["s2"] @sent-3))))))))

(deftest without-a-store-pending-stays-in-memory
  (let [r (relay-over nil (atom []) (atom []) (atom false))]
    (is (= 0 (:pending (relay/status r))))
    (is (not (contains? (relay/status r) :durable?))
        "status keeps its old shape when there is no store"))
  (let [r (relay-over (->AtomStore (atom nil)) (atom []) (atom []) (atom false))]
    (is (= {:durable? true :save-failed 0}
           (select-keys (relay/status r) [:durable? :save-failed])))))

(deftest the-edn-file-adapter-round-trips-and-tolerates-a-bad-file
  (let [dir  (doto (io/file (System/getProperty "java.io.tmpdir")
                            (str "dirge-pending-" (System/nanoTime)))
               (.mkdirs))
        path (str (io/file dir "nested" "pending.edn"))
        s    (pending-edn/edn-pending-store path)]
    (is (nil? (ports/load-pending s)) "no file yet is no pending set, not an error")
    (ports/save-pending! s (held "a" "b"))
    (is (= (held "a" "b") (ports/load-pending s)))
    (testing "a relay restarted over the file replays what it held"
      (let [sent (atom [])
            r    (relay-over s (atom []) sent (atom false))]
        (is (= 2 (relay/replay! r)))
        (is (= ["a" "b"] @sent))))
    (spit path "{:order [")
    (is (nil? (ports/load-pending s)) "an unreadable file starts empty")))
