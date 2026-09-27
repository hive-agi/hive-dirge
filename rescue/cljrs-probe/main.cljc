(ns main
  (:require [hive-addon.protocol :as p]
            [hd.probe-addon :as a]))

(defn -main [& _]
  (let [addon (a/addon-ctor {})
        init  (p/initialize! addon {:addon/id "hive.dirge.probe"})
        tool  (first (p/tools addon))
        big   (vec (range 100))
        res   ((:handler tool) {:rows big})]
    (println "satisfies?" (satisfies? p/IAddon addon))
    (println "id" (p/addon-id addon) "type" (p/addon-type addon))
    (println "caps" (p/capabilities addon))
    (println "init" init)
    (println "health" (p/health addon))
    (println "tool" (:name tool) "->" res)
    (println "shutdown" (p/shutdown! addon))))
