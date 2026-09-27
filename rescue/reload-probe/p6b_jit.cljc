(ns p6b-jit)
;; PROBE 6b: JIT redefinition, with -X debug:jit to prove the tiers engaged.
;; Also: a hot fn that CALLS a protocol method on a record, and the record is
;; re-evaluated (protocol inline-cache staleness).

(def dir "/home/klein/.claude/jobs/9d0c3547/tmp/reload-probe/src/probe/")
(defn show [label thunk]
  (println (str "  " label " => "
                (try (pr-str (thunk))
                     (catch #?(:clj Exception :default :default) e (str "THREW " (ex-message e)))))))

(defn write-h! [k]
  (spit (str dir "h2.cljc")
        (str "(ns probe.h2)\n"
             "(defprotocol P (pm [this x]))\n"
             "(defn hot [x] (+ x " k "))\n")))
(defn write-rec! [k]
  (spit (str dir "rec2.cljc")
        (str "(ns probe.rec2 (:require [probe.h2 :as h]))\n"
             "(defrecord R [] h/P (pm [_ x] (+ x " k ")))\n")))

(write-h! 1)
(require 'probe.h2)
(write-rec! 10)
(require 'probe.rec2)
(def r (probe.rec2/->R))

(defn one [] (probe.h2/hot 0))
(defn via-proto [] (probe.h2/pm r 0))
(defn driver [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc (probe.h2/hot 0) (probe.h2/pm r 0))) acc)))

(defn warm! []
  (dotimes [_ 3000] (probe.h2/hot 0) (one) (via-proto))
  (dotimes [_ 2000] (driver 10)))

(defn report [phase]
  (println "--" phase)
  (show "(one)" #(one))
  (show "(via-proto) [protocol call from hot fn]" #(via-proto))
  (show "(driver 10)" #(driver 10))
  (show "loop 3000x (one)+(via-proto)" #(loop [i 0 acc 0] (if (< i 3000) (recur (inc i) (+ acc (one) (via-proto))) acc))))

(println "== warming")
(warm!)
(report "v1 (hot k=1, pm k=10)")
(write-h! 100)
(println "== load-file h2 (hot k=100, and defprotocol P RE-EVALUATED)")
(load-file (str dir "h2.cljc"))
(in-ns 'p6b-jit)
(report "after h2 reload (protocol re-evaluated -> record R orphaned?)")
(write-rec! 1000)
(println "== load-file rec2 (pm k=1000)")
(load-file (str dir "rec2.cljc"))
(in-ns 'p6b-jit)
(report "after rec2 reload (old instance r, new bodies?)")
(warm!)
(write-rec! 7)
(println "== load-file rec2 again after re-warm (pm k=7)")
(load-file (str dir "rec2.cljc"))
(in-ns 'p6b-jit)
(report "after rec2 reload #2")
