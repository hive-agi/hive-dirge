(ns probe.h2)
(defprotocol P (pm [this x]))
(defn hot [x] (+ x 100))
