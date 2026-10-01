(ns hive-dirge.ling.process
  "IFrameTransport over a `dirge --acp` subprocess: newline-delimited
   JSON-RPC on stdin/stdout, stderr kept in a bounded ring. The binary is
   :bin, else $DIRGE_BIN, else `dirge` on PATH."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dirge.ling.acp :as acp]
            [hive-dirge.ling.ports :as p]
            [hive-dsl.result :as r])
  (:import [java.io BufferedReader BufferedWriter IOException]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent TimeUnit]))

;; SPDX-License-Identifier: MIT

(def stderr-lines-kept 200)

(defn resolve-bin
  "The dirge executable: BIN, else ENV's DIRGE_BIN, else \"dirge\"."
  [bin env]
  (or (not-empty bin) (not-empty (get env "DIRGE_BIN")) "dirge"))

(defn command
  "argv for an ACP dirge: BIN --acp EXTRA-ARGS..."
  [bin extra-args]
  (into [bin "--acp"] extra-args))

(defn parse-line
  "One stdout LINE as a frame map, a {:transport/error ..} value, or nil
   for a blank line."
  [line]
  (when-not (str/blank? line)
    (try (json/read-str line)
         (catch Exception parse-failure
           {:transport/error :acp/unparseable-line
            :line line
            :message (ex-message parse-failure)}))))

(defn- keep-line [ring line]
  (let [ring (conj ring line)]
    (if (> (count ring) stderr-lines-kept) (subvec ring 1) ring)))

(defn- daemon! [name f]
  (doto (Thread. ^Runnable f ^String name) (.setDaemon true) (.start)))

(defn- read-stdout! [^BufferedReader rdr on-frame ^Process proc]
  (let [reason (try
                 (loop []
                   (when-let [line (.readLine rdr)]
                     (some-> (parse-line line) on-frame)
                     (recur)))
                 :eof
                 (catch IOException read-failure
                   {:io-error (ex-message read-failure)}))]
    (on-frame {:transport/closed (if (= :eof reason)
                                   {:eof true :exit (when (.waitFor proc 2 TimeUnit/SECONDS)
                                                      (.exitValue proc))}
                                   reason)})))

(defn- read-stderr! [^BufferedReader rdr state]
  (try
    (loop []
      (when-let [line (.readLine rdr)]
        (swap! state update :stderr keep-line line)
        (recur)))
    (catch IOException stderr-closed
      (swap! state assoc :stderr-closed (ex-message stderr-closed)))))

(defrecord ProcessTransport [argv dir env state]
  p/IFrameTransport
  (start! [_ on-frame]
    (r/rescue (r/err :ling/spawn-failed {:argv argv})
      (let [pb (ProcessBuilder. ^java.util.List argv)
            _ (when dir (.directory pb (io/file dir)))
            _ (when (seq env) (.putAll (.environment pb) ^java.util.Map env))
            proc (.start pb)
            out (io/reader (.getInputStream proc) :encoding "UTF-8")
            err (io/reader (.getErrorStream proc) :encoding "UTF-8")
            w (BufferedWriter. (java.io.OutputStreamWriter. (.getOutputStream proc)
                                                            StandardCharsets/UTF_8))]
        (swap! state assoc :proc proc :writer w :stderr [])
        (daemon! "dirge-acp-stdout" #(read-stdout! out on-frame proc))
        (daemon! "dirge-acp-stderr" #(read-stderr! err state))
        (r/ok {:pid (.pid proc)}))))
  (send-frame! [_ frame]
    (if-let [^BufferedWriter w (:writer @state)]
      (try
        (locking w
          (.write w ^String (json/write-str frame))
          (.newLine w)
          (.flush w))
        (r/ok true)
        (catch IOException write-failure
          (r/err :ling/write-failed {:message (ex-message write-failure)})))
      (r/err :ling/not-started {})))
  (stop! [_]
    (let [[old _] (swap-vals! state dissoc :proc :writer)]
      (when-let [^Process proc (:proc old)]
        (try (.close ^BufferedWriter (:writer old))
             (catch IOException already-closed
               (swap! state assoc :close-error (ex-message already-closed))))
        (.destroy proc)
        (when-not (.waitFor proc 3 TimeUnit/SECONDS)
          (.destroyForcibly proc)))
      nil))
  (alive? [_]
    (boolean (some-> ^Process (:proc @state) .isAlive))))

(defn stderr-tail
  "The last stderr lines of TRANSPORT."
  [transport]
  (:stderr @(:state transport)))

(defn process-transport
  "A ProcessTransport for dirge. Opts: :bin, :args (extra argv), :dir,
   :env (map merged into the child environment)."
  ([] (process-transport {}))
  ([{:keys [bin args dir env]}]
   (map->ProcessTransport
    {:argv (command (resolve-bin bin (System/getenv)) args)
     :dir dir
     :env env
     :state (atom {})})))

(defn dirge-session
  "An AcpDirgeSession over a fresh dirge subprocess. OPTS is the union of
   process-transport's and hive-dirge.ling.acp/acp-session's; :cwd also
   becomes the process directory unless :dir is given."
  [{:keys [cwd dir] :as opts}]
  (acp/acp-session (process-transport (assoc opts :dir (or dir cwd))) opts))
