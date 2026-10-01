(ns hive-dirge.economy.adapters.local
  "IObservationLog over an atom, with an optional append-only EDN spill.
   The spill is fail-open: a failed write is counted, never raised."
  (:require [hive-dirge.economy.domain :as d]
            [hive-dirge.economy.ports :as p]))

(defn- runtime-fn
  [ns-str fn-str]
  (resolve (symbol ns-str fn-str)))

(defn- rust-appender
  []
  (let [parents (runtime-fn "clojure.rust.io" "make-parents")
        writer  (runtime-fn "clojure.rust.io" "writer")
        write   (runtime-fn "clojure.rust.io" "write")
        close   (runtime-fn "clojure.rust.io" "close")]
    (when (and parents writer write close)
      (fn [path text]
        (parents path)
        (let [w (writer path :append true)]
          (try (write w text) (finally (close w))))))))

(defn- jvm-appender
  []
  #?(:clj  (when-let [parents (runtime-fn "clojure.java.io" "make-parents")]
             (fn [path text]
               (parents path)
               (spit path text :append true)))
     :default nil))

(defn file-appender
  "(fn [path text]) appending to a file in this runtime, or nil."
  []
  (or (rust-appender) (jvm-appender)))

(defn spill-path
  [cwd session-id]
  (when (and cwd session-id)
    (str cwd "/.dirge/economy/" session-id ".edn")))

(defn spill-line
  [handle obs]
  (str (pr-str (assoc (select-keys obs [:tool :signature :tokens :error? :tool-use-id :body])
                      :handle handle))
       "\n"))

(defn- store
  [s full handle obs]
  (-> s
      (assoc-in [:by-handle handle] (assoc (dissoc obs :tool-use-id) :key full))
      (assoc-in [:by-key full] handle)
      (update-in [:by-body (d/body-key (:tool obs) (:body obs))] #(or % handle))))

(defn- join-id
  [s id handle]
  (if (and id handle) (assoc-in s [:by-id id] handle) s))

(defn- spill!
  [spill stats handle obs]
  (when spill
    (try
      (spill (spill-line handle obs))
      (swap! stats update :spilled (fnil inc 0))
      (catch #?(:cljs :default :default Throwable) _
        (swap! stats update :spill-errors (fnil inc 0))))))

(defrecord HiveDirgeEconomyLocalLog [state stats spill]
  p/IObservationLog
  (put-observation! [_ obs]
    (let [full (d/content-key obs)
          id   (:tool-use-id obs)
          held (get-in @state [:by-key full])]
      (if held
        (do (swap! state join-id id held) held)
        (let [s (swap! state
                       (fn [s]
                         (let [s (if (get-in s [:by-key full])
                                   s
                                   (store s full
                                          (d/mint-handle #(get-in s [:by-handle % :key]) full)
                                          obs))]
                           (join-id s id (get-in s [:by-key full])))))
              h (get-in s [:by-key full])]
          (spill! spill stats h obs)
          h))))
  (fetch [_ handle rng]
    (when-let [obs (get-in @state [:by-handle handle])]
      (d/slice rng (:body obs))))

  p/IObservationIndex
  (handle-of [_ tool body]
    (get-in @state [:by-body (d/body-key tool body)]))

  p/IObservationJoin
  (handle-by-id [_ tool-use-id]
    (when (some? tool-use-id)
      (get-in @state [:by-id (str tool-use-id)])))
  (args-of [_ handle]
    (some-> (get-in @state [:by-handle handle]) d/signature-args)))

(defn make-log
  "A LocalObservationLog. `spill` is (fn [line]) or nil."
  ([] (make-log nil))
  ([spill]
   (->HiveDirgeEconomyLocalLog (atom {:by-handle {} :by-key {}}) (atom {}) spill)))

(defn file-spill
  "(fn [line]) appending to the file `path-fn` answers at call time, or nil
   when this runtime cannot append. A nil path skips the write."
  [path-fn]
  (when-let [append! (file-appender)]
    (fn [line]
      (when-let [path (path-fn)]
        (append! path line)))))

(defn log-stats
  [log]
  (assoc @(:stats log) :observations (count (:by-handle @(:state log)))))
