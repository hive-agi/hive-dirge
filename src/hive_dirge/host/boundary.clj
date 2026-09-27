(ns hive-dirge.host.boundary
  "Effects of the dirge vessel host: the token, the 0600 discovery file, the
   hive-vessel SSE bridge and the adapter reaching hive.olympus through its
   IAddon hooks. Nothing here names a host namespace; hive.olympus is reached
   as an injected dependency instance, looked up at every call."
  (:require [clojure.java.io :as io]
            [hive-addon.protocol :as addon]
            [hive-dirge.host.domain :as domain]
            [hive-dirge.host.ports :as ports]
            [hive-vessel.executor.sse :as sse])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file CopyOption Files LinkOption OpenOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.security SecureRandom)))

;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Token
;; =============================================================================

(defn new-token
  "32 hex chars from a SecureRandom."
  []
  (let [bytes (byte-array 16)]
    (.nextBytes (SecureRandom.) bytes)
    (apply str (map #(format "%02x" (bit-and % 0xff)) bytes))))

;; =============================================================================
;; Discovery file
;; =============================================================================

(defn- perms [^String s]
  (into-array FileAttribute [(PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString s))]))

(defn write-private!
  "Atomically write CONTENT to PATH with mode 0600, creating a missing parent
   directory as 0700. The file is born 0600 (a temp file in the same
   directory, then an atomic move), so it is never readable by others, not
   even for an instant. Returns PATH."
  [path ^String content]
  (let [target (.toPath (io/file path))
        dir (.getParent target)]
    (when-not (Files/exists dir (make-array LinkOption 0))
      (Files/createDirectories dir (perms "rwx------")))
    (let [tmp (Files/createTempFile dir ".dirge" ".tmp" (perms "rw-------"))]
      (try
        (Files/writeString tmp content StandardCharsets/UTF_8 (make-array OpenOption 0))
        (Files/move tmp target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                       StandardCopyOption/REPLACE_EXISTING]))
        (finally (Files/deleteIfExists tmp))))
    path))

(defn delete-file! [path]
  (when path (Files/deleteIfExists ^Path (.toPath (io/file path)))))

;; =============================================================================
;; Bridge
;; =============================================================================

(defn refuse-every-origin
  "Origin policy: dirge is a terminal client and sends no Origin, so any
   request carrying one comes from a browser page and is refused (403)."
  [_origin]
  false)

(defn start-bridge!
  "hive-vessel's SSE bridge on loopback, on a random port unless PORT, gated
   by TOKEN, refusing every Origin. ON-REPLY receives each raw POST body."
  [{:keys [port token on-reply heartbeat-ms]}]
  (sse/start! (cond-> {:port (or port 0)
                       :token token
                       :allowed-origin? refuse-every-origin
                       :on-message on-reply}
                heartbeat-ms (assoc :heartbeat-ms heartbeat-ms))))

(defn stop-bridge! [bridge] (when bridge (sse/stop! bridge)))

(defn executor [bridge] (sse/executor bridge))

(defn bridge-status
  "What health may show about BRIDGE. Never the token."
  [bridge]
  {:port (:port bridge)
   :clients (sse/clients bridge)
   :panels (sse/retained-panels bridge)})

;; =============================================================================
;; hive.olympus through its hooks
;; =============================================================================

(def olympus-addon-id "hive.olympus")

(defn olympus-hooks
  "The live hooks map of the hive.olympus instance injected in CONFIG under
   :mount/dependencies, or {}."
  [config]
  (let [dep (get (:mount/dependencies config) olympus-addon-id)]
    (or (when (addon/addon? dep)
          (try (addon/hooks dep) (catch Throwable _ nil)))
        {})))

(defn- call-hook [hooks-fn k & args]
  (let [f (get (hooks-fn) k)]
    (if (fn? f)
      (apply f args)
      (throw (ex-info (str "hive.olympus hook unavailable: " k)
                      {:reason :olympus/unavailable :hook k})))))

(defrecord HooksOlympus [hooks-fn]
  ports/IOlympusControl
  (focus! [_ agent-id] (call-hook hooks-fn :olympus/focus! agent-id))
  (next-tab! [_] (call-hook hooks-fn :olympus/next-tab!))
  (prev-tab! [_] (call-hook hooks-fn :olympus/prev-tab!))
  (refresh! [_] (call-hook hooks-fn :olympus/refresh!)))

(defn hooks-olympus
  "IOlympusControl over HOOKS-FN (0-arity, returns an olympus hooks map),
   resolved at every call so an olympus that activates or remounts later is
   seen by the next reply."
  [hooks-fn]
  (->HooksOlympus hooks-fn))
