(ns probe.r (:require [hive-addon.protocol :as p]))
(defn make [] (reify p/IAddon (addon-id [_] "r") (health [_] {:tag :r2})))
