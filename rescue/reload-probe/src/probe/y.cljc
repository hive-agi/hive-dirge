(ns probe.y (:require [hive-addon.protocol :as p]))
(defrecord Addon [] p/IAddon (addon-id [_] "y") (health [_] {:who :y}))
(defn make [] (->Addon))
