(ns p8-stale-vars)
;; PROBE 8: a defn deleted from source survives load-file; ns-unmap / load-string availability.
(def dir "/home/klein/.claude/jobs/9d0c3547/tmp/reload-probe/src/probe/")
(defn show [label thunk]
  (println (str "  " label " => "
                (try (pr-str (thunk))
                     (catch #?(:clj Exception :default :default) e (str "THREW " (ex-message e)))))))
(spit (str dir "d.cljc") "(ns probe.d)\n(defn keep-me [] :k)\n(defn delete-me [] :still-here)\n")
(require 'probe.d)
(spit (str dir "d.cljc") "(ns probe.d)\n(defn keep-me [] :k2)\n")
(load-file (str dir "d.cljc"))
(in-ns 'p8-stale-vars)
(show "probe.d/delete-me after its defn was removed + load-file" #((resolve 'probe.d/delete-me)))
(show "(ns-unmap 'probe.d 'delete-me)" #(ns-unmap 'probe.d 'delete-me))
(show "resolve after ns-unmap" #(resolve 'probe.d/delete-me))
(show "(load-string \"(+ 1 2)\")" #(load-string "(+ 1 2)"))
(show "(keys (ns-publics 'probe.d))" #(sort (keys (ns-publics 'probe.d))))
