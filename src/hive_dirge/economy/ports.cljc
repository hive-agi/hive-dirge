(ns hive-dirge.economy.ports
  "Context-economy ports.")

(defprotocol IObservationLog
  (put-observation! [log obs]
    "Stores an Observation; answers its Handle (stable for equal content).")
  (fetch [log handle rng]
    "Text of the Observation under `handle` sliced by `rng`, or nil."))

(defprotocol IObservationIndex
  (handle-of [log tool body]
    "Handle of an Observation already logged for `tool` with exactly `body`
     (whatever its args), or nil."))

(defprotocol IDigestor
  (digest [d span anchor prior-citations budget]
    "Digest map of `span` (see hive-dirge.economy.digest) whose markdown fits
     `budget` chars (nil: no limit), carrying `prior-citations` forward; nil
     when it cannot, never a throw."))
