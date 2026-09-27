(ns driver2)

(def f "/home/klein/.claude/jobs/9d0c3547/tmp/dirge-slice-reload/src/probe/addon.cljc")

(clojure.core/load-file driver2/f)
(clojure.core/println "first:" ((clojure.core/resolve 'probe.addon/hello))
                      ((clojure.core/resolve 'probe.addon/describe) ((clojure.core/resolve 'probe.addon/->Thing) 1)))

(clojure.core/spit driver2/f
  (clojure.core/str "(ns probe.addon)\n"
                    "(defprotocol IThing (describe [this]))\n"
                    "(defrecord Thing [n] IThing (describe [this] (str \"v2:\" n)))\n"
                    "(defn hello [] \"hello-v2\")\n"
                    "(defn added [] :new-fn)\n"))
(clojure.core/load-file driver2/f)
(clojure.core/println "second:" ((clojure.core/resolve 'probe.addon/hello))
                      ((clojure.core/resolve 'probe.addon/describe) ((clojure.core/resolve 'probe.addon/->Thing) 2))
                      ((clojure.core/resolve 'probe.addon/added)))
