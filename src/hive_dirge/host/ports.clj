(ns hive-dirge.host.ports
  "Ports of the dirge vessel host.

   IOlympusControl is what a dirge reply drives. The production adapter
   (hive-dirge.host.boundary/hooks-olympus) reaches hive.olympus through its
   IAddon hooks; tests pass a recording implementation through config."
  )

;; SPDX-License-Identifier: MIT

(defprotocol IOlympusControl
  (focus! [port agent-id] "Focus AGENT-ID; nil returns to the grid.")
  (next-tab! [port])
  (prev-tab! [port])
  (refresh! [port]))

(defn route!
  "Apply COMMAND (a hive-dirge.host.domain command value) to PORT. Returns
   {:routed command-kw :result r}, or the error value unchanged when COMMAND
   is one. A port method that throws becomes {:reply/error :reply/port-threw}."
  [port {:keys [command agent-id] :as cmd}]
  (if (:reply/error cmd)
    cmd
    (try
      {:routed command
       :result (case command
                 :olympus/focus (focus! port agent-id)
                 :olympus/unfocus (focus! port nil)
                 :olympus/next-tab (next-tab! port)
                 :olympus/prev-tab (prev-tab! port)
                 :olympus/refresh (refresh! port))}
      (catch Throwable t
        {:reply/error :reply/port-threw :command command :message (ex-message t)}))))
