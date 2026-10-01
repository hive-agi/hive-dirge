(ns hive-dirge.ling.process-test
  "Pure helpers, the IFrameTransport contract (fake always, dirge when
   DIRGE_BIN is set), and a live ACP session against DIRGE_BIN.
   DIRGE_LIVE_PROMPT=1 also runs one model turn with a tool call."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dirge.ling.fake-agent :as fake]
            [hive-dirge.ling.ports :as p]
            [hive-dirge.ling.process :as proc]
            [hive-dsl.result :as r]))

;; SPDX-License-Identifier: MIT

(def dirge-bin (not-empty (System/getenv "DIRGE_BIN")))

(deftest resolve-bin-order
  (is (= "/x/dirge" (proc/resolve-bin "/x/dirge" {"DIRGE_BIN" "/y"})))
  (is (= "/y" (proc/resolve-bin nil {"DIRGE_BIN" "/y"})))
  (is (= "/y" (proc/resolve-bin "" {"DIRGE_BIN" "/y"})))
  (is (= "dirge" (proc/resolve-bin nil {}))))

(deftest command-argv
  (is (= ["dirge" "--acp"] (proc/command "dirge" nil)))
  (is (= ["d" "--acp" "--model" "m"] (proc/command "d" ["--model" "m"]))))

(deftest parse-line-cases
  (is (nil? (proc/parse-line "   ")))
  (is (= {"id" 1 "result" {}} (proc/parse-line "{\"id\":1,\"result\":{}}")))
  (let [bad (proc/parse-line "not json")]
    (is (= :acp/unparseable-line (:transport/error bad)))
    (is (= "not json" (:line bad)))))

(deftest missing-binary-fails-start
  (let [t (proc/process-transport {:bin "/nonexistent/dirge-acp-test"})]
    (is (= :ling/spawn-failed (:error (p/start! t (fn [_])))))
    (is (not (p/alive? t)))))

;; =============================================================================
;; IFrameTransport contract
;; =============================================================================

(defn- transport-contract [make-transport]
  (let [t (make-transport)]
    (testing "send before start is refused"
      (is (r/err? (p/send-frame! t {"jsonrpc" "2.0" "method" "x"}))))
    (testing "start, then alive"
      (is (r/ok? (p/start! t (fn [_]))))
      (is (p/alive? t)))
    (testing "stop is idempotent and final"
      (p/stop! t)
      (p/stop! t)
      (is (not (p/alive? t)))
      (is (r/err? (p/send-frame! t {"jsonrpc" "2.0" "method" "x"}))))))

(deftest scripted-agent-meets-transport-contract
  (transport-contract #(fake/scripted-agent {})))

(deftest ^:integration process-transport-meets-transport-contract
  (if-not dirge-bin
    (is true "DIRGE_BIN unset; skipped")
    (transport-contract #(proc/process-transport {:bin dirge-bin}))))

;; =============================================================================
;; Live dirge
;; =============================================================================

(defn- temp-dir []
  (str (.toFile (java.nio.file.Files/createTempDirectory
                 "hive-dirge-acp" (make-array java.nio.file.attribute.FileAttribute 0)))))

(deftest ^:integration live-open-and-close
  (if-not dirge-bin
    (is true "DIRGE_BIN unset; skipped")
    (let [dir (temp-dir)
          commands (promise)
          s (proc/dirge-session {:bin dirge-bin :cwd dir :timeout-ms 30000
                                 :on-event #(when (= :ling/commands (:event %))
                                              (deliver commands %))})]
      (try
        (let [opened (p/open! s)]
          (is (r/ok? opened) (pr-str opened (proc/stderr-tail (:transport s))))
          (is (string? (:session-id (:ok opened))))
          (is (seq (:commands (deref commands 10000 nil)))))
        (finally
          (p/close! s)
          (is (not (p/alive? (:transport s))))
          (io/delete-file dir true))))))

(deftest ^:integration live-tool-turn
  (if-not (and dirge-bin (System/getenv "DIRGE_LIVE_PROMPT"))
    (is true "DIRGE_BIN or DIRGE_LIVE_PROMPT unset; skipped")
    (let [dir (temp-dir)
          s (proc/dirge-session {:bin dirge-bin :cwd dir :timeout-ms 30000})]
      (try
        (is (r/ok? (p/open! s)))
        (is (r/ok? (p/prompt! s (str "Use your bash tool to run exactly `echo acp-probe` once, "
                                     "then reply with the single word: done"))))
        (let [done (p/collect! s 180000)
              summary (p/events s)
              outputs (keep :output (vals (:tools summary)))]
          (is (r/ok? done) (pr-str done))
          (is (= :end-turn (:stop-reason (:ok done))))
          (is (some #(str/includes? % "acp-probe") outputs))
          (is (every? #{:completed :failed} (map :status (vals (:tools summary))))))
        (finally
          (p/close! s)
          (io/delete-file dir true))))))
