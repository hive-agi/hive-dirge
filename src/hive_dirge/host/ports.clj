(ns hive-dirge.host.ports
  "Ports of the dirge vessel host.

   IOlympusControl is what a dirge reply drives. The production adapter
   (hive-dirge.host.boundary/hooks-olympus) reaches hive.olympus through its
   IAddon hooks; tests pass a recording implementation through config.

   InvokeRouter owns dirge's invoke replies: the action \"invoke\" carries a
   panel id, a verb, a cursor row and a payload. The registry-backed adapter
   (hive-dirge.host.boundary/registry-invoke-router) asks the lens registry
   who owns the panel; when the owning lens lives in-dirge the reply is
   re-routed over the :dirge/invoke hook, when the host owns it the verb
   runs here, and an unknown panel or verb is ignored with a warning — never
   a wire error, so dirge keeps its defaults.

   IActionQueue holds accepted reply commands until they run, one at a time
   and in arrival order. The production adapter
   (hive-dirge.host.boundary/single-worker-queue) is one worker thread over a
   bounded FIFO; tests pass a stub through config (:dirge/action-queue).")

;; SPDX-License-Identifier: MIT

(defprotocol IOlympusControl
  (focus! [port agent-id] "Focus AGENT-ID; nil returns to the grid.")
  (next-tab! [port])
  (prev-tab! [port])
  (refresh! [port]))

(defprotocol IActionQueue
  (submit! [queue command]
    "Accept COMMAND to be run later, after every command accepted before it.
     True when accepted, false when the queue is full or closed.")
  (close! [queue] "Stop accepting; commands not yet started are dropped."))

(defprotocol InvokeRouter
  (route-invoke! [router invoke]
    "Apply one invoke {:command :invoke :invoke {\"panel\" .. \"verb\" .. \"row\" .. \"payload\" ..}}.
     True when the verb ran, false when it was ignored (unknown panel or
     verb). Never throws: a bad invoke is a log line, not a wire error."))

(defn route!
  "Apply COMMAND (a hive-dirge.host.domain command value) to PORT. An invoke
   command goes to ROUTER (an InvokeRouter) when one is given, else to PORT
   when it satisfies InvokeRouter. Returns {:routed command-kw :result r},
   or the error value unchanged when COMMAND is one. A port method that
   throws becomes {:reply/error :reply/port-threw}."
  ([port command] (route! port nil command))
  ([port router {:keys [command agent-id] :as cmd}]
   (if (:reply/error cmd)
     cmd
     (try
       {:routed command
        :result (case command
                  :olympus/focus (focus! port agent-id)
                  :olympus/unfocus (focus! port nil)
                  :olympus/next-tab (next-tab! port)
                  :olympus/prev-tab (prev-tab! port)
                  :olympus/refresh (refresh! port)
                  :invoke (route-invoke! (or router port) cmd))}
       (catch Throwable t
         {:reply/error :reply/port-threw :command command :message (ex-message t)})))))
