(ns hive-dirge.sense.pending-edn
  "IPendingStore over one EDN file, by default next to the sixth-sense
   snapshot (~/.local/share/hive-agent/). A save writes a sibling temp file
   and moves it over the target, so a crash mid-write leaves the previous
   set, never half of one. An absent or unreadable file loads as nil: the
   relay then starts empty, as it did before it had a store."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hive-dirge.sense.ports :as ports])
  (:import (java.nio.file CopyOption Files StandardCopyOption)))

;; SPDX-License-Identifier: MIT

(defn default-path
  "Where the relay's pending set lives unless config names a path."
  []
  (str (System/getProperty "user.home") "/.local/share/hive-agent/dirge-sense-pending.edn"))

(defn- read-file [f]
  (try (edn/read-string (slurp f))
       (catch Exception _unreadable-file-loads-as-nothing nil)))

(defrecord EdnPendingStore [path]
  ports/IPendingStore
  (load-pending [_]
    (let [f (io/file path)]
      (when (.isFile f) (read-file f))))
  (save-pending! [_ pending]
    (let [f   (io/file path)
          tmp (io/file (str path ".tmp"))]
      (io/make-parents f)
      (spit tmp (pr-str pending))
      (Files/move (.toPath tmp) (.toPath f)
                  (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING
                                          StandardCopyOption/ATOMIC_MOVE]))
      nil)))

(defn edn-pending-store
  "An EdnPendingStore at PATH (default-path when omitted)."
  ([] (edn-pending-store (default-path)))
  ([path] (->EdnPendingStore (str path))))
