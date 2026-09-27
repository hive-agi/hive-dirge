(ns probe.addon)

(defprotocol IThing
  (describe [this]))

(defrecord Thing [n]
  IThing
  (describe [this] (str "v1:" n)))

(defn hello [] "hello-v1")
