(ns probe.s2 (:require [hive-addon.protocol :as p]))
(defonce state (atom {:loads 0}))
(swap! state update :loads inc)
(def plain-state (atom :fresh))
(defrecord S2Addon [cfg]
  p/IAddon
  (addon-id [_] "probe.s2")
  (initialize! [this c] (swap! state assoc :init :v2) {:success? true})
  (shutdown! [_] (swap! state assoc :shutdown :v2) nil)
  (tools [_] [{:name "t" :handler (fn [x] [:v2 x (:loads @state)])}])
  (health [_] {:status :ok :tag :v2}))
(defn make [] (->S2Addon {}))
