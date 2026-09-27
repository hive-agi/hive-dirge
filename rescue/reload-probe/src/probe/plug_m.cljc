(ns probe.plug-m (:require [probe.host-m :as h]))
(defmethod h/render :ui/show-panel [m] [:plugin-panel m])
