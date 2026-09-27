(ns rf.main (:require [hive-addon.protocol :as p]))

(defmacro try-show [label expr]
  `(println ~label (try (pr-str ~expr) (catch #?(:clj Throwable :default :default) e# (str "THREW " (ex-message e#))))))

;; 1. collision of same simple record name across ns
(require 'rf.xa)
(def x1 ((resolve 'rf.xa/make)))
(try-show "x1 id before ya loaded:" (p/addon-id x1))
(require 'rf.ya)
(try-show "x1 id after ya loaded:" (p/addon-id x1))
(try-show "fresh xa/make id:" (p/addon-id ((resolve 'rf.xa/make))))

;; 2. JIT: protocol call on unimplemented type in a hot fn
(defrecord Bare [])
(defn via [x] (p/addon-id x))
(try-show "cold via Bare:" (via (->Bare)))
(dotimes [_ 5000] (via x1))
(try-show "hot via Bare:" (via (->Bare)))
(try-show "hot via {}:" (via {}))

;; 3. hive-addon optional-method catch path under JIT
(try-show "unimplemented-method? available:" (boolean (resolve 'hive-addon.protocol/unimplemented-method?)))

;; 4. require :reload
(try-show "require :reload:" (require 'rf.xa :reload))
