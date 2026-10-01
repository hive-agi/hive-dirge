(ns hive-dirge.ling.fake-agent
  "In-memory IFrameTransport stand-ins for an ACP agent: a scripted agent
   (method -> handler) and a replay agent driven by a recorded frame log."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dirge.ling.ports :as p]
            [hive-dsl.result :as r]))

;; SPDX-License-Identifier: MIT

(defn load-recording
  "[[:out|:in frame] ...] from a JSONL fixture on the classpath-relative PATH."
  [path]
  (->> (str/split-lines (slurp (io/file path)))
       (remove str/blank?)
       (mapv (fn [line] (let [{:strs [dir frame]} (json/read-str line)]
                          [(keyword dir) frame])))))

(defrecord ScriptedAgent [handlers state]
  p/IFrameTransport
  (start! [_ on-frame]
    (swap! state assoc :on-frame on-frame :alive? true :starts (inc (:starts @state 0)))
    (r/ok {:pid 0}))
  (send-frame! [this frame]
    (if-not (:alive? @state)
      (r/err :ling/not-started {})
      (do (swap! state update :received conj frame)
          (when-let [h (get handlers (or (get frame "method") :response))]
            (h this frame))
          (r/ok true))))
  (stop! [_]
    (swap! state #(-> % (assoc :alive? false) (update :stops (fnil inc 0))))
    nil)
  (alive? [_] (boolean (:alive? @state))))

(defn scripted-agent
  "A ScriptedAgent answering frames with HANDLERS: method string (or
   :response for a reply to an agent request) -> (fn [agent frame])."
  [handlers]
  (->ScriptedAgent handlers (atom {:received []})))

(defn deliver!
  "Push FRAME from AGENT to the client."
  [agent frame]
  ((:on-frame @(:state agent)) frame))

(defn received
  "Frames the client sent to AGENT."
  [agent]
  (:received @(:state agent)))

(defn reply! [agent frame result]
  (deliver! agent {"jsonrpc" "2.0" "id" (get frame "id") "result" result}))

(defn update! [agent sid upd]
  (deliver! agent {"jsonrpc" "2.0" "method" "session/update"
                   "params" {"sessionId" sid "update" upd}}))

(defn- replies-by-method
  "method -> the recorded incoming frames that followed that request."
  [recording]
  (loop [[[dir frame] & more] recording current nil acc {}]
    (cond
      (nil? dir) acc
      (and (= :out dir) (get frame "method") (get frame "id"))
      (recur more (get frame "method") (assoc acc (get frame "method") []))
      (and (= :in dir) current)
      (recur more current (update acc current conj frame))
      :else (recur more current acc))))

(defn replay-agent
  "A ScriptedAgent that answers each request method with the frames the
   RECORDING shows after it, response ids rewritten to the live request id."
  [recording]
  (let [by-method (replies-by-method recording)]
    (scripted-agent
     (into {}
           (map (fn [[method frames]]
                  [method (fn [agent req]
                            (doseq [f frames]
                              (deliver! agent (if (contains? f "method")
                                                f
                                                (assoc f "id" (get req "id"))))))]))
           by-method))))
