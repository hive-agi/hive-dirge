(ns p7b-recipe)
;; PROBE 7b: host-style hot-reload recipe (fixed ns restore) + ns leak inside a fn body.
(require '[hive-addon.protocol :as p])
(def dir "/home/klein/.claude/jobs/9d0c3547/tmp/reload-probe/src/probe/")
(defn show [label thunk]
  (println (str "  " label " => "
                (try (pr-str (thunk))
                     (catch #?(:clj Exception :default :default) e (str "THREW " (ex-message e)))))))

(defn write-s! [tag]
  (spit (str dir "s2.cljc")
        (str "(ns probe.s2 (:require [hive-addon.protocol :as p]))\n"
             "(defonce state (atom {:loads 0}))\n"
             "(swap! state update :loads inc)\n"
             "(def plain-state (atom :fresh))\n"
             "(defrecord S2Addon [cfg]\n"
             "  p/IAddon\n"
             "  (addon-id [_] \"probe.s2\")\n"
             "  (initialize! [this c] (swap! state assoc :init :" tag ") {:success? true})\n"
             "  (shutdown! [_] (swap! state assoc :shutdown :" tag ") nil)\n"
             "  (tools [_] [{:name \"t\" :handler (fn [x] [:" tag " x (:loads @state)])}])\n"
             "  (health [_] {:status :ok :tag :" tag "}))\n"
             "(defn make [] (->S2Addon {}))\n")))

(show "(str *ns*) vs (ns-name *ns*)" #(vector (str *ns*) (ns-name *ns*)))

(def registry (atom {}))
(defn register! [ctor-sym]
  (let [ctor (deref (resolve ctor-sym))
        inst (ctor)
        init (p/initialize! inst {})]
    (swap! registry assoc (p/addon-id inst)
           {:inst inst :tools (into {} (map (juxt :name :handler)) (p/tools inst)) :init init})))
(defn call-tool [id tname x] ((get-in @registry [id :tools tname]) x))

(defn leak-test [file]
  (load-file file)
  ;; next form in the SAME fn body: does `registry` (p7b-recipe var) still resolve?
  [(str *ns*) (count @registry)])

(defn reload-addon! [id file ctor-sym]
  (let [old (get-in @registry [id :inst])
        saved (ns-name *ns*)]
    (p/shutdown! old)
    (load-file file)
    (in-ns saved)
    (register! ctor-sym)))

(write-s! "v1")
(require 'probe.s2)
(register! 'probe.s2/make)
(def stale-handler (get-in @registry ["probe.s2" :tools "t"]))
(show "call-tool v1" #(call-tool "probe.s2" "t" 1))
(show "leak-test (load-file inside fn, then use own-ns var)" #(leak-test (str dir "s2.cljc")))
(in-ns 'p7b-recipe)
(write-s! "v2")
(show "reload-addon! v2" #(do (reload-addon! "probe.s2" (str dir "s2.cljc") 'probe.s2/make) :ok))
(show "call-tool after reload" #(call-tool "probe.s2" "t" 1))
(show "stale handler captured before reload" #(stale-handler 1))
(show "defonce state survives (loads, init, shutdown)" #(deref @(resolve 'probe.s2/state)))
(show "plain def atom after reload" #(deref @(resolve 'probe.s2/plain-state)))
(show "health of instance now in registry" #(p/health (get-in @registry ["probe.s2" :inst])))
(show "*ns* after reload-addon!" #(ns-name *ns*))
