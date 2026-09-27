(ns p1-defn)
;; PROBE 1+2: defn redefinition via require :reload, load-file, eval/read-string;
;; captured fn values vs var indirection vs closures in a tool :handler map.

(def dir "/home/klein/.claude/jobs/9d0c3547/tmp/reload-probe/src/probe/")

(defn write-a! [tag]
  (spit (str dir "a.cljc")
        (str "(ns probe.a)\n"
             "(println \"  [loading probe.a " tag "]\")\n"
             "(defn f [x] [:" tag " x])\n"
             "(defn g [x] (f x))\n")))

(defn show [label thunk]
  (println (str "  " label " => "
                (try (pr-str (thunk))
                     (catch #?(:clj Exception :default :default) e (str "THREW " (ex-message e)))))))

(write-a! "v1")
(require 'probe.a)
(spit (str dir "b.cljc")
      "(ns probe.b (:require [probe.a :as a]))\n(defn call-f [x] (a/f x))\n")
(require 'probe.b)

;; captures made while v1 is live
(def cap-value   probe.a/f)                         ; fn value
(def cap-var     #'probe.a/f)                       ; var
(def tool-value  {:handler probe.a/f})              ; handler map, value
(def tool-var    {:handler #'probe.a/f})            ; handler map, var
(def tool-clos   {:handler (fn [x] (probe.a/f x))}) ; closure that names the var
(def cap-partial (partial probe.a/f))
(def cap-comp    (comp identity probe.a/f))

(defn report [phase]
  (println "--" phase)
  (show "direct (probe.a/f 1)" #(probe.a/f 1))
  (show "intra-ns (probe.a/g 1)" #(probe.a/g 1))
  (show "other-ns caller (probe.b/call-f 1)" #(probe.b/call-f 1))
  (show "cap-value" #(cap-value 1))
  (show "cap-var (#'f)" #(cap-var 1))
  (show "tool-value :handler" #((:handler tool-value) 1))
  (show "tool-var :handler" #((:handler tool-var) 1))
  (show "tool-clos :handler" #((:handler tool-clos) 1))
  (show "partial" #(cap-partial 1))
  (show "comp" #(cap-comp 1))
  (show "identical? cap-value @#'probe.a/f" #(identical? cap-value @#'probe.a/f))
  (show "identical? cap-var #'probe.a/f" #(identical? cap-var #'probe.a/f)))

(report "after v1 require")

(write-a! "v2")
(show "(require 'probe.a) again, no :reload" #(require 'probe.a))
(report "after plain re-require (no :reload), file now v2")

(show "(require 'probe.a :reload)" #(require 'probe.a :reload))
(report "after require :reload (v2)")

(write-a! "v3")
(show "(load-file a.cljc)" #(load-file (str dir "a.cljc")))
(report "after load-file (v3)")

(write-a! "v4")
(show "(require 'probe.a :reload-all)" #(require 'probe.a :reload-all))
(report "after require :reload-all (v4)")

;; eval of new source at runtime (no file)
(show "eval read-string defn v5"
      #(eval (read-string "(do (in-ns 'probe.a) (defn f [x] [:v5-eval x]))")))
(in-ns 'p1-defn)
(report "after eval of (defn f ...) in probe.a (v5)")

(show "eval with fully qualified def via binding *ns*"
      #(binding [*ns* (find-ns 'probe.a)] (eval '(defn f [x] [:v6-eval-binding x]))))
(report "after eval under (binding [*ns* probe.a]) (v6)")

(show "load-string exists?" #(resolve 'load-string))
(show "alter-var-root f -> v7" #(alter-var-root #'probe.a/f (fn [_] (fn [x] [:v7-avr x]))))
(report "after alter-var-root (v7)")

;; remove-ns and re-require
(show "remove-ns probe.a" #(remove-ns 'probe.a))
(show "find-ns probe.a after remove-ns" #(find-ns 'probe.a))
(show "cap-var after remove-ns" #(cap-var 1))
(show "probe.b/call-f after remove-ns" #(probe.b/call-f 1))
(write-a! "v8")
(show "(require 'probe.a) after remove-ns" #(require 'probe.a))
(report "after remove-ns + require (v8)")
