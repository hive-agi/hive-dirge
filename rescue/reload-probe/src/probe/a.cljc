(ns probe.a)
(println "  [loading probe.a v8]")
(defn f [x] [:v8 x])
(defn g [x] (f x))
