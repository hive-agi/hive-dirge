(ns hive-dirge.sense.ports
  "Port of the sense relay: where senses come from.

   ISenseSource is sixth-sense seen from the dirge host: a push wake-up
   (`listen!`) plus a consume-once drain under one consumer id. The
   production adapter (hive-dirge.sense.relay/sixth-sense-source) resolves
   hive-agent.sixth-sense.api at call time, since hive-agent is a sibling
   addon and not a dependency; tests pass a recording implementation.")

;; SPDX-License-Identifier: MIT

(defprotocol ISenseSource
  (listen! [source key f]
    "Call (f sense) after every new sense. Same KEY replaces.")
  (unlisten! [source key])
  (drain! [source consumer receptor]
    "Senses CONSUMER has not been handed yet, through RECEPTOR (nil = the
     source's tuning); consume-once. -> [Sense]"))
