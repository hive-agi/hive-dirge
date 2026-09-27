(ns t1)
(doseq [s '[future future-call pmap agent send promise deliver pcalls thread clojure.core.async/thread
            Thread/sleep locking]]
  (println s "=>" (try (resolve s) (catch #?(:clj Exception :default :default) e (str "ERR " (ex-message e))))))
