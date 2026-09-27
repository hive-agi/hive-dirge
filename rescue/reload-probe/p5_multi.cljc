(ns p5-multi
  (:require [hive-addon.protocol :as p]))
;; PROBE 5: defmulti/defmethod reload; plus reify-based addon reload; plus
;; which ns top-level forms land in after load-file.

(def dir "/home/klein/.claude/jobs/9d0c3547/tmp/reload-probe/src/probe/")
(defn show [label thunk]
  (println (str "  " label " => "
                (try (pr-str (thunk))
                     (catch #?(:clj Exception :default :default) e (str "THREW " (ex-message e)))))))

(defn write-m! [tag dispatch methods]
  (spit (str dir "m.cljc")
        (str "(ns probe.m)\n(println \"  [loading probe.m " tag "]\")\n"
             "(defmulti op " dispatch ")\n"
             methods)))

(write-m! "v1" ":kind"
          "(defmethod op :a [x] [:v1-a x])\n(defmethod op :b [x] [:v1-b x])\n(defmethod op :default [x] [:v1-default x])\n")
(require 'probe.m)
(def cap-multi probe.m/op)
(def tool {:handler probe.m/op})
(defn report [phase]
  (println "--" phase)
  (show "(probe.m/op {:kind :a})" #(probe.m/op {:kind :a}))
  (show "(probe.m/op {:kind :b})" #(probe.m/op {:kind :b}))
  (show "(probe.m/op {:kind :c})" #(probe.m/op {:kind :c}))
  (show "(probe.m/op {:type :a})  [dispatch fn probe]" #(probe.m/op {:type :a}))
  (show "cap-multi {:kind :a}" #(cap-multi {:kind :a}))
  (show "tool :handler {:kind :b}" #((:handler tool) {:kind :b}))
  (show "(keys (methods probe.m/op))" #(sort (map str (keys (methods probe.m/op)))))
  (show "(identical? cap-multi probe.m/op)" #(identical? cap-multi probe.m/op)))
(report "v1")

;; v2: same dispatch fn, :a body changed, :b REMOVED from source
(write-m! "v2" ":kind" "(defmethod op :a [x] [:v2-a x])\n(defmethod op :default [x] [:v2-default x])\n")
(show "load-file m.cljc (v2: :a changed, :b removed)" #(load-file (str dir "m.cljc")))
(in-ns 'p5-multi)
(report "after v2 load-file")

;; v3: dispatch fn CHANGED to :type
(write-m! "v3" ":type" "(defmethod op :a [x] [:v3-a x])\n")
(show "load-file m.cljc (v3: dispatch fn -> :type)" #(load-file (str dir "m.cljc")))
(in-ns 'p5-multi)
(report "after v3 load-file (dispatch changed)")

;; standalone defmethod eval
(show "eval (defmethod probe.m/op :z ...)" #(eval '(defmethod probe.m/op :z [x] [:eval-z x])))
(show "(probe.m/op {:type :z})" #(probe.m/op {:type :z}))
(show "remove-method :a" #(remove-method probe.m/op :a))
(show "(probe.m/op {:type :a}) after remove-method" #(probe.m/op {:type :a}))

(println "== reify-based addon reload")
(spit (str dir "r.cljc")
      "(ns probe.r (:require [hive-addon.protocol :as p]))\n(defn make [] (reify p/IAddon (addon-id [_] \"r\") (health [_] {:tag :r1})))\n")
(require 'probe.r)
(def r-old (probe.r/make))
(show "r-old health" #(p/health r-old))
(show "r-old type" #(type r-old))
(spit (str dir "r.cljc")
      "(ns probe.r (:require [hive-addon.protocol :as p]))\n(defn make [] (reify p/IAddon (addon-id [_] \"r\") (health [_] {:tag :r2})))\n")
(show "load-file r.cljc" #(load-file (str dir "r.cljc")))
(in-ns 'p5-multi)
(def r-new (probe.r/make))
(show "r-old health after reload" #(p/health r-old))
(show "r-new health after reload" #(p/health r-new))
(show "types" #(vector (type r-old) (type r-new)))
(show "r-old unimplemented method (tools)" #(p/tools r-old))

(println "== where do top-level defs land after load-file?")
(spit (str dir "q.cljc") "(ns probe.q)\n(def q 1)\n")
(show "*ns* before" #(str *ns*))
(load-file (str dir "q.cljc"))
(def landed 42)
(show "*ns* after load-file (no in-ns)" #(str *ns*))
(show "resolve p5-multi/landed" #(resolve 'p5-multi/landed))
(show "resolve probe.q/landed" #(resolve 'probe.q/landed))
