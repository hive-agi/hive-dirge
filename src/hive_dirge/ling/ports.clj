(ns hive-dirge.ling.ports
  "Ports of a dirge ling.

   IDirgeSession is one hive ling seen as a dirge session: open it, prompt
   it, cancel or collect the running turn, read what it produced and what it
   cost, close it. hive-dirge.ling.acp/AcpDirgeSession implements it over
   ACP; backends and tests depend on this protocol only.

   IFrameTransport is the wire under an ACP session: JSON-RPC frames as
   string-keyed maps, one listener for incoming frames. The production
   adapter is hive-dirge.ling.process (a `dirge --acp` subprocess on stdio);
   tests pass an in-memory fake agent.

   Fallible operations answer a hive-dsl Result: {:ok v} or {:error kw ...}.")

;; SPDX-License-Identifier: MIT

(defprotocol IDirgeSession
  (open! [session]
    "Start the agent, initialize ACP and create a session. -> Result {:session-id s}")
  (prompt! [session text]
    "Send one prompt turn; returns at once. -> Result {:turn n}, or an error
     when a turn is already running or the session is not open.")
  (cancel! [session]
    "Ask the agent to stop the running turn. -> Result true")
  (collect! [session timeout-ms]
    "Block until the running turn ends or TIMEOUT-MS elapses. -> Result of
     the :ling/turn-end event, or {:error :ling/timeout}.")
  (transcript [session]
    "Every event received so far, oldest first. -> [Event]")
  (cost [session]
    "Accumulated usage. -> {:input-tokens .. :output-tokens .. :cost-usd ..}")
  (events [session]
    "The folded session summary (hive-dirge.ling.domain/empty-summary shape).")
  (close! [session]
    "End the session and release the transport. Idempotent. -> Result true"))

(defprotocol IFrameTransport
  (start! [transport on-frame]
    "Begin delivering incoming frames to (ON-FRAME frame); a frame that
     failed to parse arrives as {:transport/error kw :line s}. -> Result")
  (send-frame! [transport frame]
    "Write one frame. -> Result")
  (stop! [transport]
    "Release the transport. Idempotent.")
  (alive? [transport]))
