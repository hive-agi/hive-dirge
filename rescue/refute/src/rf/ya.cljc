(ns rf.ya (:require [hive-addon.protocol :as p]))
(defrecord Addon [] p/IAddon (addon-id [_] "ya") (health [_] {:who :ya}))
(defn make [] (->Addon))
