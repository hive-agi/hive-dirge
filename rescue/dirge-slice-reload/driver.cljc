(ns driver)

(def f "/home/klein/.claude/jobs/9d0c3547/tmp/dirge-slice-reload/src/probe/addon.cljc")

(load-file f)
(println "first:" ((resolve 'probe.addon/hello))
         ((resolve 'probe.addon/describe) ((resolve 'probe.addon/->Thing) 1)))

(spit f (str "(ns probe.addon)\n"
             "(defprotocol IThing (describe [this]))\n"
             "(defrecord Thing [n] IThing (describe [this] (str \"v2:\" n)))\n"
             "(defn hello [] \"hello-v2\")\n"
             "(defn added [] :new-fn)\n"))
(load-file f)
(println "second:" ((resolve 'probe.addon/hello))
         ((resolve 'probe.addon/describe) ((resolve 'probe.addon/->Thing) 2))
         ((resolve 'probe.addon/added)))
