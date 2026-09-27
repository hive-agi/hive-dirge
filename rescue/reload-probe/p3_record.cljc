(ns p3-record
  (:require [hive-addon.protocol :as p]))
;; PROBE 3+4: defrecord re-eval (old vs new instances), then defprotocol re-eval
;; using the REAL hive-addon.protocol/IAddon.

(def dir "/home/klein/.claude/jobs/9d0c3547/tmp/reload-probe/src/probe/")
(def proto-file "/home/klein/PP/hive/hive-addon/src/hive_addon/protocol.cljc")

(defn show [label thunk]
  (println (str "  " label " => "
                (try (pr-str (thunk))
                     (catch #?(:clj Exception :default :default) e (str "THREW " (ex-message e)))))))

(defn write-addon! [tag extra-field?]
  (spit (str dir "addon.cljc")
        (str "(ns probe.addon (:require [hive-addon.protocol :as p]))\n"
             "(println \"  [loading probe.addon " tag "]\")\n"
             "(defrecord Addon [state" (when extra-field? " extra") "]\n"
             "  p/IAddon\n"
             "  (addon-id [_] \"probe.addon\")\n"
             "  (addon-type [_] :native)\n"
             "  (capabilities [_] #{:tools})\n"
             "  (initialize! [this cfg] (swap! state assoc :init :" tag ") {:success? true :tag :" tag "})\n"
             "  (shutdown! [_] (swap! state assoc :shutdown :" tag ") nil)\n"
             "  (tools [_] [{:name \"t\" :handler (fn [params] [:" tag " params])}])\n"
             "  (schema-extensions [_] [])\n"
             "  (health [this] {:status :ok :tag :" tag (when extra-field? " :extra (:extra this)") "})\n"
             "  (excluded-tools [_] #{})\n"
             "  (hooks [_] {}))\n"
             "(defn make [] (map->Addon {:state (atom {})" (when extra-field? " :extra :x") "}))\n")))

(defn probe-inst [label a]
  (println "  --" label)
  (show "    (p/health a)" #(p/health a))
  (show "    (p/addon? a)  [satisfies?]" #(p/addon? a))
  (show "    (satisfies? p/IAddon a)" #(satisfies? p/IAddon a))
  (show "    (p/initialize! a {})" #(p/initialize! a {}))
  (show "    ((:handler (first (p/tools a))) :x)" #((:handler (first (p/tools a))) :x))
  (show "    (type a)" #(type a))
  (show "    (record? a)" #(record? a)))

(write-addon! "v1" false)
(require 'probe.addon)
(def old (probe.addon/make))
(def old-tools (p/tools old))
(def old-type (type old))
(def old-ctor probe.addon/->Addon)
(println "== after v1 load")
(probe-inst "old instance" old)

(println "== reload probe.addon (v2 same fields) via load-file")
(write-addon! "v2" false)
(show "load-file addon.cljc" #(load-file (str dir "addon.cljc")))
(show "*ns* after load-file (does load-file restore ns?)" #(str *ns*))
(def new2 (probe.addon/make))
(probe-inst "OLD instance (built by v1 ctor)" old)
(probe-inst "NEW instance (v2 ctor)" new2)
(show "captured old-tools handler (built before reload)" #((:handler (first old-tools)) :x))
(show "old-ctor (captured ->Addon v1) instance health" #(p/health (old-ctor (atom {}))))
(show "(= (type old) (type new2))" #(= (type old) (type new2)))
(show "(= old-type (type new2))" #(= old-type (type new2)))
(show "(instance? (type new2) old)" #(instance? (type new2) old))
(show "(= old (map->Addon same fields)) value equality across reload"
      #(let [s (atom {})] (= (old-ctor s) (probe.addon/->Addon s))))

(println "== reload probe.addon (v3 ADDS a field :extra) via load-file")
(write-addon! "v3" true)
(show "load-file addon.cljc" #(load-file (str dir "addon.cljc")))
(def new3 (probe.addon/make))
(probe-inst "OLD instance (v1, no :extra field)" old)
(probe-inst "NEW instance (v3)" new3)
(show "(:extra old)" #(:extra old))
(show "(keys old) / (keys new3)" #(vector (keys old) (keys new3)))

(println "== PROBE 4: re-evaluate defprotocol (load-file hive_addon/protocol.cljc)")
(def proto-before p/IAddon)
(def health-fn-before p/health)
(show "load-file protocol.cljc" #(load-file proto-file))
(in-ns 'p3-record)
(show "(identical? proto-before p/IAddon)" #(identical? proto-before p/IAddon))
(show "(= proto-before p/IAddon)" #(= proto-before p/IAddon))
(show "(identical? health-fn-before p/health)" #(identical? health-fn-before p/health))
(probe-inst "OLD instance (v1) after protocol reload" old)
(probe-inst "new3 instance (v3, built before protocol reload)" new3)
(show "captured health-fn-before on new3" #(health-fn-before new3))
(show "(satisfies? proto-before new3)" #(satisfies? proto-before new3))
(println "== reload probe.addon (v4) AFTER protocol reload")
(write-addon! "v4" true)
(show "load-file addon.cljc" #(load-file (str dir "addon.cljc")))
(def new4 (probe.addon/make))
(probe-inst "NEW instance (v4, after proto reload)" new4)
(probe-inst "new3 instance again (after v4 reload)" new3)
(show "captured health-fn-before on new4" #(health-fn-before new4))
(show "(satisfies? proto-before new4)" #(satisfies? proto-before new4))

(println "== extend-type after the fact on a plain map type")
(show "extend-type PersistentArrayMap? use (type {})" #(type {}))
