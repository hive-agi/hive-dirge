(ns hive-dirge.host.boundary
  "Effects of the dirge vessel host: the token, the 0600 discovery file, the
   hive-vessel SSE bridge with its /reply route answered here, the single
   worker running reply commands in order, and the adapter reaching
   hive.olympus through its IAddon hooks. Nothing here names a host namespace; hive.olympus is reached
   as an injected dependency instance, looked up at every call."
  (:require [clojure.java.io :as io]
            [hive-addon.protocol :as addon]
            [hive-dirge.host.domain :as domain]
            [hive-dirge.host.ports :as ports]
            [hive-vessel.executor.sse :as sse])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.io InputStream)
           (java.nio.charset StandardCharsets)
           (java.nio.file CopyOption Files LinkOption OpenOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.security SecureRandom)
           (java.util.concurrent ArrayBlockingQueue RejectedExecutionException
                                 ThreadFactory ThreadPoolExecutor TimeUnit)))

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

(def reply-path (str domain/route-prefix "/reply"))

(defn- read-bounded
  "At most LIMIT bytes of IN as UTF-8, or nil when the body is longer."
  [^InputStream in limit]
  (let [bytes (.readNBytes in (int (inc limit)))]
    (when (<= (alength bytes) limit)
      (String. bytes StandardCharsets/UTF_8))))

(defn- origin-admitted?
  "A request with no Origin is a terminal client; any Origin goes through
   refuse-every-origin."
  [origin]
  (or (nil? origin) (refuse-every-origin origin)))

(defn- answer! [^HttpExchange ex status]
  (try
    (.sendResponseHeaders ex (int status) -1)
    (finally (.close ex))))

(defn- reply-handler
  "The /reply route: admission, a bounded body read, then ON-BODY (raw ->
   HTTP status) decides the answer. Nothing here waits on olympus."
  [token on-body]
  (reify HttpHandler
    (handle [_ ex]
      (try
        (let [params (sse/query-params (.getRawQuery (.getRequestURI ex)))
              refusal (domain/reply-admission
                       {:origin-allowed? (origin-admitted? (.getFirst (.getRequestHeaders ex) "Origin"))
                        :token-ok? (sse/token-matches? token (get params "token"))
                        :method (.getRequestMethod ex)})]
          (if refusal
            (answer! ex refusal)
            (if-let [raw (read-bounded (.getRequestBody ex) domain/max-reply-bytes)]
              (answer! ex (on-body raw))
              (answer! ex 413))))
        (catch Throwable _
          (try (answer! ex 500) (catch Throwable _ nil)))))))

(defn start-bridge!
  "hive-vessel's SSE bridge on loopback, on a random port unless PORT, gated
   by TOKEN, refusing every Origin. Its /reply route is replaced by ours:
   ON-REPLY receives each admitted raw POST body and returns the HTTP status
   to answer, so it must not block on the action it accepts."
  [{:keys [port token on-reply heartbeat-ms]}]
  (let [bridge (sse/start! (cond-> {:port (or port 0)
                                    :token token
                                    :allowed-origin? refuse-every-origin}
                             heartbeat-ms (assoc :heartbeat-ms heartbeat-ms)))
        ^HttpServer server (:server bridge)]
    (.removeContext server ^String reply-path)
    (.createContext server ^String reply-path ^HttpHandler (reply-handler token on-reply))
    bridge))

(defn stop-bridge! [bridge] (when bridge (sse/stop! bridge)))

(defn executor [bridge] (sse/executor bridge))

(defn bridge-status
  "What health may show about BRIDGE. Never the token."
  [bridge]
  {:port (:port bridge)
   :clients (sse/clients bridge)
   :panels (sse/retained-panels bridge)})

;; =============================================================================
;; Reply commands: one worker, arrival order
;; =============================================================================

(defn- worker-threads []
  (reify ThreadFactory
    (newThread [_ r]
      (doto (Thread. ^Runnable r "hive-dirge-reply")
        (.setDaemon true)))))

(defrecord SingleWorkerQueue [^ThreadPoolExecutor pool run-command]
  ports/IActionQueue
  (submit! [_ command]
    (try
      (.execute pool ^Runnable (fn [] (try (run-command command) (catch Throwable _ nil))))
      true
      (catch RejectedExecutionException _ false)))
  (close! [_]
    (.shutdownNow pool)
    nil))

(defn single-worker-queue
  "IActionQueue over one daemon thread and a FIFO of CAPACITY waiting
   commands; RUN-COMMAND is applied to each command, one at a time, in submission
   order. A full or closed queue refuses (submit! answers false)."
  [{:keys [capacity run-command]}]
  (->SingleWorkerQueue (ThreadPoolExecutor. 1 1 0 TimeUnit/MILLISECONDS
                                            (ArrayBlockingQueue. (int capacity))
                                            ^ThreadFactory (worker-threads))
                       run-command))

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
