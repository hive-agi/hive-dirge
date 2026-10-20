(ns hive-dirge.lens.carto-test
  "Lens L1 over stub ports: the pure neighborhood layout, the pull through
   :mcp-call, and the recenter/open verbs. No live carto."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dirge.hive.addon :as addon]
            [hive-dirge.lens.carto :as carto]
            [hive-dirge.lens.registry :as lens]))

(def callers-data
  {:callers [{:qn "a.core/caller" :file "src/a/core.clj" :line 10}
             {:qn "a.core/caller" :file "src/a/core.clj" :line 10}
             {:id "uuid-external" :qn nil}
             {:qn "b.x/other"}]})

(def callees-data
  {:callees [{:qn "c.y/leaf" :file "src/c/y.clj" :line 3}]})

(deftest neighbors-drop-unresolved-and-duplicates
  (is (= [{:qn "a.core/caller" :file "src/a/core.clj" :line 10} {:qn "b.x/other"}]
         (carto/neighbors callers-data :callers)))
  (is (= [] (carto/neighbors nil :callers)))
  (is (= [] (carto/neighbors {:callers "nope"} :callers))))

(deftest neighborhood-layers-callers-center-callees
  (let [cs (carto/neighbors callers-data :callers)
        ds (carto/neighbors callees-data :callees)
        n  (carto/neighborhood "m/center" cs ds)]
    (is (= ["a.core/caller" "b.x/other" "m/center" "c.y/leaf"] (mapv :id (:nodes n))))
    (is (= [["a.core/caller" "m/center"] ["b.x/other" "m/center"] ["m/center" "c.y/leaf"]]
           (:edges n)))
    (is (= ["a.core/caller" "b.x/other" "m/center" "c.y/leaf"] (mapv :id (:rows n)))
        "row ids are qns, callers first")
    (is (= {:qn "c.y/leaf" :file "src/c/y.clj" :line 3} (:payload (peek (:rows n)))))
    (testing "doc carries a dag block and the rows"
      (let [d (carto/doc "m/center" n 2 1)]
        (is (= [:para :dag] (mapv :block/type (:doc/blocks d))))
        (is (= (:rows n) (:lens/rows d)))))))

(defn- stub-ports [calls panels]
  {:mcp-call   (fn [server tool args]
                 (swap! calls conj [server tool args])
                 {:content [{:type "text" :text (get args "command")}]})
   :json-parse (fn [text] (if (= text "carto callers") callers-data callees-data))
   :panel!     (fn [op] (swap! panels conj op) true)
   :cwd        (constantly "/w/p")})

(deftest open-pulls-callers-and-callees-through-ports
  (let [calls (atom []) panels (atom [])
        out (carto/open-lens (stub-ports calls panels) {:hive/server "hive"}
                             {:argv ["carto" "m/center"]})]
    (is (= [["hive" "code" {"command" "carto callers" "function" "m/center" "depth" 1 "directory" "/w/p"}]
            ["hive" "code" {"command" "carto callees" "function" "m/center" "depth" 1 "directory" "/w/p"}]]
           @calls))
    (is (= "carto m/center: 2 callers, 1 callees (side panel)" (:text out)))
    (let [op (first (:fx out))]
      (is (= "carto" (:panel/id op)))
      (is (= true (:cursor op)))
      (is (= {"invoke" "recenter"} (get-in op [:keys "enter"])))
      (is (= 4 (count (:panel/rows op))))
      (is (some #(= :dag (:block/type %)) (get-in op [:doc :doc/blocks]))))))

(deftest open-without-qn-is-usage
  (is (re-find #"usage" (:text (carto/open-lens (stub-ports (atom []) (atom [])) {} {:argv ["carto"]})))))

(deftest verbs-recenter-and-open
  (let [calls (atom []) panels (atom [])
        ports (stub-ports calls panels)
        reg   (addon/default-registry)]
    (is (some? (lens/lookup reg :carto)) "carto is in the addon's default registry")
    ((:effects (lens/route-invoke reg {:panel "carto" :verb "recenter" :row "c.y/leaf"})) ports)
    (is (= "c.y/leaf" (get-in (first @calls) [2 "function"])))
    (is (= "carto" (:panel/id (first @panels))))
    (reset! panels [])
    ((:effects (lens/route-invoke reg {:panel "carto" :verb "open" :row "c.y/leaf"
                                       :payload {"file" "src/c/y.clj" "line" 3}})) ports)
    (is (= [{:op :ui/open-file :path "src/c/y.clj" :line 3}] @panels))
    (testing "a row without a file opens nothing"
      (is (nil? (carto/open-file-op {:qn "x/y"}))))))

(deftest recenter-honours-the-config-port
  (let [calls (atom []) panels (atom [])
        ports (assoc (stub-ports calls panels) :config (constantly {:hive/server "hive-dev"}))]
    ((:effects (lens/route-invoke (addon/default-registry)
                                  {:panel "carto" :verb "recenter" :row "c.y/leaf"})) ports)
    (is (= "hive-dev" (ffirst @calls)) "the configured server, not the default")))

(deftest run-command-3-arity-resolves-carto
  (testing "/hive carto resolves through run-command without make-addon"
    (let [calls (atom []) panels (atom [])
          out (addon/run-command (stub-ports calls panels) {:hive/server "hive"}
                                 {:argv ["carto" "m/center"]})]
      (is (= "carto m/center: 2 callers, 1 callees (side panel)" (:text out)))
      (is (= ["carto"] (mapv :panel/id @panels)))))
  (testing "the 3-arity lists the same lenses make-addon mounts"
    (is (re-find #"/hive carto"
                 (:text (addon/run-command (stub-ports (atom []) (atom [])) {} {:argv ["lens"]}))))))
