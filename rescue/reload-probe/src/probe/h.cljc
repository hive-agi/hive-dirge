(ns probe.h)
(defn hot [x] (+ x 100))
(defn hot-kw [m] (assoc m :v 100))
