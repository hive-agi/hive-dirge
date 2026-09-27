(ns probe.s (:require [hive-addon.protocol :as p]))
(defonce state (atom {:loads 0}))
(swap! state update :loads inc)
(def plain-state (atom :fresh))
(defrecord SAddon [cfg]
  p/IAddon
  (addon-id [_] "probe.s")
  (initialize! [this c] (swap! state assoc :init :v2) {:success? true})
  (shutdown! [_] (swap! state assoc :shutdown :v2) nil)
  (tools [_] [{:name "t" :handler (fn [x] [:v2 x (:loads @state)])}])
  (health [_] {:status :ok :tag :v2}))
(defn make [] (->SAddon {}))
