(ns probe.u (:require [hive-addon.protocol :as p]))
(defrecord Thing [] p/IAddon (addon-id [_] "u"))
