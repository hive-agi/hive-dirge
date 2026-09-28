(ns hive-dirge.economy.adapters.structured
  "IDigestor built from the span alone, with no model call: extraction, carry
   forward and budget fitting from hive-dirge.economy.digest."
  (:require [hive-dirge.economy.digest :as dg]
            [hive-dirge.economy.ports :as p]))

(defrecord HiveDirgeEconomyStructuredDigestor []
  p/IDigestor
  (digest [_ span anchor prior-citations budget]
    (try
      (dg/fit (dg/build span anchor prior-citations) budget)
      (catch #?(:cljs :default :default Throwable) _
        nil))))

(defn make-digestor
  [_config]
  (->HiveDirgeEconomyStructuredDigestor))
