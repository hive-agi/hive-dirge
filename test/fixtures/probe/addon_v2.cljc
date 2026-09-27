(ns probe.addon)

(defprotocol IThing
  (describe [this]))

(defrecord Thing [n]
  IThing
  (describe [this] (str "v2:" n)))

(defn hello [] "hello-v2")

(defn added [] :new-fn)
