(ns hive-dirge.host.ports
  "Ports of the dirge vessel host.

   IOlympusControl is what a dirge reply drives. The production adapter
   (hive-dirge.host.boundary/hooks-olympus) reaches hive.olympus through its
   IAddon hooks; tests pass a recording implementation through config.

   route-command is the open registry of reply commands (command keyword ->
   what it does to the port). The five olympus commands and :invoke are
   registered here; an addon adds one with defmethod from its own namespace.

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

(defmulti route-command
  "The reply command registry: command keyword -> what it does to PORT.

   Dispatches on (:command cmd). The five olympus commands are registered
   below by default; an addon adds a reply command by extending this method
   from its own namespace, never by editing route!:

     (defmethod ports/route-command :my-addon/do-thing [port cmd]
       (do-thing! port (:target cmd)))

   The method returns the port's result, which route! reports as :result.
   It may throw; route! turns that into {:reply/error :reply/port-threw}.
   Parsing a wire action into a command value is hive-dirge.host.domain's
   job and is not covered here."
  (fn [_port cmd] (:command cmd)))

(defmethod route-command :olympus/focus [port {:keys [agent-id]}] (focus! port agent-id))
(defmethod route-command :olympus/unfocus [port _] (focus! port nil))
(defmethod route-command :olympus/next-tab [port _] (next-tab! port))
(defmethod route-command :olympus/prev-tab [port _] (prev-tab! port))
(defmethod route-command :olympus/refresh [port _] (refresh! port))

(defmethod route-command :invoke [port cmd]
  (route-invoke! (or (::router cmd) port) (dissoc cmd ::router)))

(defmethod route-command :default [_ {:keys [command]}]
  (throw (ex-info (str "No route registered for command " (pr-str command))
                  {:command command})))

(defn registered-commands
  "The set of command keywords route-command has a method for."
  []
  (disj (set (keys (methods route-command))) :default))

(defn route!
  "Apply COMMAND (a hive-dirge.host.domain command value) to PORT through the
   route-command registry. An invoke command goes to ROUTER (an InvokeRouter)
   when one is given, else to PORT when it satisfies InvokeRouter. Returns
   {:routed command-kw :result r}, or the error value unchanged when COMMAND
   is one. A method that throws, or a command with no registered method,
   becomes {:reply/error :reply/port-threw}."
  ([port command] (route! port nil command))
  ([port router {:keys [command] :as cmd}]
   (if (:reply/error cmd)
     cmd
     (try
       {:routed command
        :result (route-command port (cond-> cmd router (assoc ::router router)))}
       (catch Throwable t
         {:reply/error :reply/port-threw :command command :message (ex-message t)})))))
