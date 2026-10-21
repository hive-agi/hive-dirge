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

(defprotocol IPendingStore
  "Where the relay keeps ops it sent but dirge has not acked.

   drain! is consume-once, so an op held only in memory is gone after a hive
   restart between send and ack. A store outlives the JVM: the relay saves
   the held set on every change and seeds itself from it at start!. The
   production adapter is hive-dirge.sense.pending-edn (one EDN file next to
   the sixth-sense snapshot); nil means in memory only."
  (load-pending [store]
    "The pending set last saved, as saved, or nil when there is none. The
     relay normalizes it (hive-dirge.sense.domain/restore), so a store need
     not validate.")
  (save-pending! [store pending]
    "Replace the saved pending set with PENDING ({:order [id] :ops {id op}})."))
