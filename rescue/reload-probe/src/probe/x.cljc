(ns probe.x (:require [hive-addon.protocol :as p]))
(defrecord Addon [] p/IAddon (addon-id [_] "x") (health [_] {:who :x}))
(defn make [] (->Addon))
