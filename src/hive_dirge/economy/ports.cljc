(ns hive-dirge.economy.ports
  "Context-economy ports.")

(defprotocol IObservationLog
  (put-observation! [log obs]
    "Stores an Observation; answers its Handle (stable for equal content).")
  (fetch [log handle rng]
    "Text of the Observation under `handle` sliced by `rng`, or nil."))
