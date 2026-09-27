(ns hd.probe-addon
  (:require [hive-addon.protocol :as p]))

(defrecord ProbeAddon [state]
  p/IAddon
  (addon-id [_] "hive.dirge.probe")
  (addon-type [_] :native)
  (capabilities [_] #{:tools :dirge/hooks :dirge/panels})
  (initialize! [_ config] (reset! state config) {:success? true :errors [] :metadata {}})
  (shutdown! [_] (reset! state nil) {:success? true})
  (tools [_] [{:name "swarm-view"
               :description "probe"
               :inputSchema {:type "object"}
               :handler (fn [params] {:content [{:type "text" :text (str "rows=" (count (:rows params)))}]})}])
  (schema-extensions [_] {})
  (health [_] {:status :ok :details {:config @state}}))

(defn addon-ctor [_config] (->ProbeAddon (atom nil)))
