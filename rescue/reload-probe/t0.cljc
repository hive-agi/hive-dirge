(ns t0)
(doseq [s '[spit slurp load-file load-string eval remove-ns read-string ns-unmap find-ns
            extend-protocol extend-type satisfies? remove-method methods prefer-method
            loaded-libs *loaded-libs* require load alter-var-root ns-publics ns-interns
            create-ns in-ns defmulti get-method remove-all-methods extends? type instance?]]
  (println s "=>" (try (resolve s) (catch #?(:clj Exception :default :default) e (str "ERR " e)))))
(println "version" *clojure-version*)
