(ns p6-jit)
;; PROBE 6: does JIT / IR tiering leave stale code after a hot fn is redefined?

(def dir "/home/klein/.claude/jobs/9d0c3547/tmp/reload-probe/src/probe/")
(defn show [label thunk]
  (println (str "  " label " => "
                (try (pr-str (thunk))
                     (catch #?(:clj Exception :default :default) e (str "THREW " (ex-message e)))))))

(defn write-h! [k]
  (spit (str dir "h.cljc")
        (str "(ns probe.h)\n"
             "(defn hot [x] (+ x " k "))\n"
             "(defn hot-kw [m] (assoc m :v " k "))\n")))

(write-h! 1)
(require 'probe.h)

;; driver: its own loop calls the var many times -> driver itself gets hot too
(defn driver [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc (probe.h/hot 0))) acc)))
(defn one [] (probe.h/hot 0))
(defn one-kw [] (:v (probe.h/hot-kw {})))
(def cap-hot probe.h/hot)
(def tool {:handler (fn [x] (probe.h/hot x))})

(defn warm! []
  (dotimes [_ 5000] (probe.h/hot 0) (one) (one-kw) (cap-hot 0) ((:handler tool) 0))
  (dotimes [_ 3000] (driver 10)))

(defn report [phase]
  (println "--" phase)
  (show "(probe.h/hot 0)" #(probe.h/hot 0))
  (show "(one) [hot caller]" #(one))
  (show "(one-kw)" #(one-kw))
  (show "(driver 10) [loop sum]" #(driver 10))
  (show "cap-hot (captured value)" #(cap-hot 0))
  (show "tool :handler closure" #((:handler tool) 0))
  (show "in-loop sum of (one) x 2000" #(loop [i 0 acc 0] (if (< i 2000) (recur (inc i) (+ acc (one))) acc))))

(warm!)
(report "v1 after warm-up (5000+ calls)")

(write-h! 100)
(show "load-file h.cljc (k=100)" #(load-file (str dir "h.cljc")))
(report "v2 immediately after redefinition")
(warm!)
(report "v2 after re-warm")

(show "eval (defn probe.h/hot ...) k=1000 via in-ns"
      #(eval (read-string "(do (in-ns 'probe.h) (defn hot [x] (+ x 1000)))")))
(in-ns 'p6-jit)
(report "v3 after eval redefinition")

(show "alter-var-root hot -> k=5" #(alter-var-root #'probe.h/hot (fn [_] (fn [x] (+ x 5)))))
(report "v4 after alter-var-root")

(println "-- background thread hammering (one) while main thread redefines")
(def seen (atom #{}))
(def stop (atom false))
(def fut (future (loop [n 0] (if @stop n (do (swap! seen conj (one)) (recur (inc n)))))))
(Thread/sleep 50)
(write-h! 7)
(load-file (str dir "h.cljc"))
(in-ns 'p6-jit)
(Thread/sleep 50)
(reset! stop true)
(show "background iterations" #(deref fut))
(show "distinct values seen by background loop" #(sort @seen))
(report "v5 after background-run redefinition")
