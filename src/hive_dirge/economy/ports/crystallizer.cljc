(ns hive-dirge.economy.ports.crystallizer
  "Crystallization port: where epoch Digests outlive the session.")

(defprotocol ICrystallizer
  (crystallize! [c entry]
    "Files a Digest memory entry (hive-dirge.economy.crystal/entry); truthy
     when it was filed, never a throw.")
  (seeds [c directory n]
    "Texts of the newest `n` Digests filed for `directory`, newest first;
     [] when there are none or the store is unreachable, never a throw."))
