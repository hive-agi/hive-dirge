(ns probe.rec2 (:require [probe.h2 :as h]))
(defrecord R [] h/P (pm [_ x] (+ x 7)))
