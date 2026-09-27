(ns rf.xa (:require [hive-addon.protocol :as p]))
(defrecord Addon [] p/IAddon (addon-id [_] "xa") (health [_] {:who :xa}))
(defn make [] (->Addon))
