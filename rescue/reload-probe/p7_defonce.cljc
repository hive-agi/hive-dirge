(ns p7-defonce)
;; PROBE 7: state preservation across reload (defonce), and a full host-style
;; hot-reload recipe against the real IAddon protocol.
(require '[hive-addon.protocol :as p])
(def dir "/home/klein/.claude/jobs/9d0c3547/tmp/reload-probe/src/probe/")
(defn show [label thunk]
  (println (str "  " label " => "
                (try (pr-str (thunk))
                     (catch #?(:clj Exception :default :default) e (str "THREW " (ex-message e)))))))

(defn write-s! [tag]
  (spit (str dir "s.cljc")
        (str "(ns probe.s (:require [hive-addon.protocol :as p]))\n"
             "(defonce state (atom {:loads 0}))\n"
             "(swap! state update :loads inc)\n"
             "(def plain-state (atom :fresh))\n"
             "(defrecord SAddon [cfg]\n"
             "  p/IAddon\n"
             "  (addon-id [_] \"probe.s\")\n"
             "  (initialize! [this c] (swap! state assoc :init :" tag ") {:success? true})\n"
             "  (shutdown! [_] (swap! state assoc :shutdown :" tag ") nil)\n"
             "  (tools [_] [{:name \"t\" :handler (fn [x] [:" tag " x (:loads @state)])}])\n"
             "  (health [_] {:status :ok :tag :" tag "}))\n"
             "(defn make [] (->SAddon {}))\n")))

;; host-side registry: holds instance + tool map, looked up per call
(def registry (atom {}))
(defn register! [ctor-sym]
  (let [ctor (deref (resolve ctor-sym))
        inst (ctor)
        init (p/initialize! inst {})]
    (swap! registry assoc (p/addon-id inst)
           {:inst inst :tools (into {} (map (juxt :name :handler)) (p/tools inst)) :init init})))
(defn call-tool [id tname x] ((get-in @registry [id :tools tname]) x))

(defn reload-addon! [id file ctor-sym]
  (let [old (get-in @registry [id :inst])
        saved-ns *ns*]
    (p/shutdown! old)                       ; 1. old instance releases resources (old bodies? new?)
    (load-file file)                        ; 2. re-eval source
    (in-ns (symbol (str saved-ns)))         ; 3. undo load-file's ns leak
    (register! ctor-sym)))                  ; 4. fresh ctor + initialize! + re-read tools

(write-s! "v1")
(require 'probe.s)
(register! 'probe.s/make)
(def stale-handler (get-in @registry ["probe.s" :tools "t"]))
(show "call-tool v1" #(call-tool "probe.s" "t" 1))
(write-s! "v2")
(show "reload-addon! v2" #(do (reload-addon! "probe.s" (str dir "s.cljc") 'probe.s/make) :ok))
(show "call-tool after reload" #(call-tool "probe.s" "t" 1))
(show "stale handler captured before reload" #(stale-handler 1))
(show "defonce state (loads counted, init/shutdown tags)" #(deref @(resolve 'probe.s/state)))
(show "plain def atom after reload" #(deref @(resolve 'probe.s/plain-state)))
(show "*ns* after reload-addon!" #(str *ns*))
