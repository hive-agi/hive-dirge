(ns hive-dirge.economy.pipeline.crystallize
  "crystallize: a Digest the compact hook answered -> Promote a memory entry
   -> Boundary ICrystallizer. seed: session-start -> Boundary seeds -> Promote
   the context text. Both are opt-in (:economy/crystallize? true) and
   fail-open: no crystallizer, no write, no seed."
  (:require [hive-dirge.economy.crystal :as crystal]
            [hive-dirge.economy.ports.crystallizer :as p]))

(defn enabled?
  [config]
  (true? (:economy/crystallize? config)))

(defn after-compact!
  "Files the Digest in `answer` ({:summary md} or nil) through `crystallizer`
   and counts it in `stats`. Answers `answer` unchanged, so it can wrap the
   compact hook."
  [{:keys [crystallizer stats config]} ctx answer]
  (when-let [e (and crystallizer (enabled? config)
                    (crystal/entry (:summary answer)
                                   {:session-id (:session-id ctx)
                                    :cwd        (:cwd ctx)
                                    :epoch      (get-in @stats [:last-digest :epoch])}))]
    (swap! stats update (if (p/crystallize! crystallizer e) :crystals :crystal-errors)
           (fnil inc 0)))
  answer)

(defn seed
  "{:context text} with the newest Digests filed for `cwd`, or nil."
  [{:keys [crystallizer config]} cwd]
  (when (and crystallizer (enabled? config))
    (let [n (or (:economy/seed-limit config) crystal/default-seed-limit)]
      (when-let [text (crystal/seed-context (p/seeds crystallizer cwd n) n
                                            (or (:economy/seed-chars config)
                                                crystal/default-seed-chars))]
        {:context text}))))
