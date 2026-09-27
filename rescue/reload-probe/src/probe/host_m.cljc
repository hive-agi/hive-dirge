(ns probe.host-m)
(defmulti render :op)
(defmethod render :default [m] [:host-default m])
