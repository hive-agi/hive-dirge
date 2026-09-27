(ns probe.m)
(println "  [loading probe.m v3]")
(defmulti op :type)
(defmethod op :a [x] [:v3-a x])
