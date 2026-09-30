(ns hive-dirge.economy.digest
  "Digest: the structured, REFERENCE-ONLY epoch summary that replaces a folded
   span. Pure.

   Digest = {:epoch int :at str|nil
             :request str|nil   latest user message in the span, verbatim
             :task str|nil      Anchor: first user task statement, verbatim
             :plan [line]       Anchor: TODO / checkbox lines, verbatim, latest state
             :open [line]       unchecked subset of :plan
             :done [line]       temporal, past-tense done list (E<epoch>.<step>)
             :files [path] :decisions [line] :errors [line]
             :citations [{:handle h :line str}]}

   A span entry is {:role \"user\"|\"assistant\"|\"tool\" :text str :tool str?
   :tool-use-id str? :args str? :handle str?}, or a \"system\" entry holding a
   rendered Digest; an entry whose text is a rendered Digest is
   never re-summarized: its lines are carried through as they are."
  (:require [clojure.string :as str]
            [hive-dirge.economy.markdown :as md]
            [hive-dirge.economy.domain :as d]))

;; ---------------------------------------------------------------------------
;; Small text helpers

(defn- lines
  [s]
  (if (string? s) (str/split s #"\n") []))

(defn clip
  [s n]
  (if (> (count s) n) (str (subs s 0 n) "…") s))

(defn- first-line
  [s]
  (or (some (fn [l] (when-not (str/blank? l) (str/trim l))) (lines s)) ""))

(defn dedupe-by
  "First occurrence of each (f x), order kept."
  [f xs]
  (:out (reduce (fn [{:keys [seen] :as st} x]
                  (let [k (f x)]
                    (if (contains? seen k)
                      st
                      (-> st (update :seen conj k) (update :out conj x)))))
                {:seen #{} :out []}
                xs)))

(defn- keep-last
  [n xs]
  (let [v (vec xs)]
    (subvec v (max 0 (- (count v) n)))))

;; ---------------------------------------------------------------------------
;; Span

(defn- entry
  [i m]
  (when (map? m)
    (let [text (:text m)
          text (cond (string? text) text (nil? text) "" :else (pr-str text))
          r    (:role m)
          role (cond (string? r) r (keyword? r) (name r) :else nil)
          dig? (md/digest-text? text)
          args (d/args-text (:args m))]
      (when (or (contains? #{"user" "assistant" "tool"} role)
                (and (= "system" role) dig?))
        (cond-> {:i i :role role :text text}
          (:tool m)        (assoc :tool (str (:tool m)))
          (:tool-use-id m) (assoc :tool-use-id (str (:tool-use-id m)))
          (:handle m)      (assoc :handle (str (:handle m)))
          args             (assoc :args args)
          dig?             (assoc :digest (md/parse text)))))))

(defn normalize-span
  "Well-formed entries of `span`, in order; anything else is dropped."
  [span]
  (if (sequential? span)
    (vec (keep identity (map-indexed entry span)))
    []))

(defn tool-entries
  [entries]
  (filterv #(and (= "tool" (:role %)) (not (:digest %))) entries))

(defn- fresh
  [entries]
  (remove :digest entries))

(defn- priors
  [entries]
  (keep :digest entries))

;; ---------------------------------------------------------------------------
;; Anchor

(def plan-line-re #"^\s*(?:[-*]\s+\[[ xX]\]\s+\S|TODO\b|[-*]\s+TODO\b)")

(defn plan-lines
  [text]
  (vec (keep (fn [l] (when (re-find plan-line-re l) (str/trim l))) (lines text))))

(defn- plan-key
  [line]
  (-> line
      (str/replace #"^\s*[-*]?\s*(?:\[[ xX]\]\s*)?(?:TODO:?\s*)?" "")
      str/trim
      str/lower-case))

(defn merge-plan
  "Latest state per plan item, first-seen order."
  [ls]
  (let [{:keys [order by]} (reduce (fn [st l]
                                     (let [k (plan-key l)]
                                       (-> st
                                           (update :order #(if (contains? (:by st) k) % (conj % k)))
                                           (assoc-in [:by k] l))))
                                   {:order [] :by {}}
                                   ls)]
    (mapv by order)))

(defn open-items
  [plan]
  (filterv #(re-find #"\[ \]|^\s*[-*]?\s*TODO\b" %) plan))

(defn anchor-of
  "{:task :plan}: the first task statement (a prior Digest's Goal wins over
   the span's first user message) and the merged TODO/plan state."
  [entries]
  (let [ps    (priors entries)
        users (filter #(= "user" (:role %)) (fresh entries))]
    {:task (or (some :task ps)
               (some (fn [e] (when-not (str/blank? (:text e)) (:text e))) users))
     :plan (merge-plan (concat (mapcat :plan ps)
                               (mapcat #(plan-lines (:text %))
                                       (filter #(contains? #{"user" "assistant"} (:role %))
                                               (fresh entries)))))}))

;; ---------------------------------------------------------------------------
;; Extraction

(def ^:private file-re
  #"(?:[A-Za-z0-9_.-]+/)+[A-Za-z0-9_-][A-Za-z0-9_.-]*\.[A-Za-z0-9]{1,8}")

(defn files-in
  [text]
  (->> (str/replace text #"[A-Za-z][A-Za-z0-9+.-]*://\S+" " ")
       (re-seq file-re)
       (remove #(str/starts-with? % "."))))

(def ^:private decision-re
  #"(?i)\b(?:decided|decision|chose|chosen|going with|will use|opted|switched to|instead of)\b")

(defn decisions-in
  [text]
  (keep (fn [l] (when (re-find decision-re l) (clip (str/trim l) 200))) (lines text)))

(def ^:private error-re
  #"(?i)\b(?:error|exception|failed|failure|panic|traceback|denied|fatal)\b")

(defn- cite
  [h]
  (str "§" h))

(defn step-id
  [epoch k]
  (str "E" epoch "." k))

(def done-args-max-chars 120)

(defn done-line
  [epoch at k {:keys [tool args text handle]}]
  (let [n  (count (remove str/blank? (lines text)))
        pv (clip (first-line text) 80)]
    (str (step-id epoch k) (when at (str " (" at ")"))
         " ran " (or tool "a tool")
         (when-not (str/blank? args) (str " " (clip args done-args-max-chars)))
         ": " n (if (= 1 n) " line" " lines")
         (when-not (str/blank? pv) (str ", first \"" pv "\""))
         (when handle (str " [" (cite handle) "]")))))

(defn error-line
  [epoch k {:keys [tool text handle]}]
  (when-let [l (some (fn [l] (when (re-find error-re l) l)) (lines text))]
    (str (step-id epoch k) " " (or tool "tool") ": " (clip (str/trim l) 160)
         (when handle (str " [" (cite handle) "]")))))

(defn citation
  [{:keys [tool text handle]}]
  (when handle
    {:handle handle
     :line   (str (cite handle) " " (or tool "tool") ", ~" (quot (+ (count text) 3) 4)
                  " tok: " (clip (first-line text) 80)
                  " (context_retrieve " (cite handle) ")")}))

(defn short-citation
  [{:keys [handle] :as c}]
  (assoc c :line (str (cite handle) " (context_retrieve " (cite handle) ")")))

;; ---------------------------------------------------------------------------
;; Build

(defn build
  "The Digest of `span` (entries as normalize-span answers, raw maps are
   normalized too). `anchor` is {:task :plan :prior-epoch :at} (nil: derived
   from the span); `prior-citations` are carried forward before the span's
   own. nil when the span holds nothing to digest."
  [span anchor prior-citations]
  (let [entries (if (and (sequential? span) (every? :i span)) (vec span) (normalize-span span))
        ps      (vec (priors entries))
        fr      (vec (fresh entries))
        derived (anchor-of entries)
        anchor  (merge derived (select-keys anchor [:prior-epoch :at])
                       (when (:task anchor) {:task (:task anchor)})
                       (when (seq (:plan anchor)) {:plan (:plan anchor)}))
        epoch   (inc (reduce max (or (:prior-epoch anchor) 0) (map :epoch ps)))
        at      (:at anchor)
        steps   (map-indexed (fn [k e] [(inc k) e]) (tool-entries entries))
        request (or (some (fn [e] (when (and (= "user" (:role e)) (not (str/blank? (:text e))))
                                    (:text e)))
                          (reverse fr))
                    (some :request (reverse ps)))
        digest  {:epoch     epoch
                 :at        at
                 :request   request
                 :task      (:task anchor)
                 :plan      (vec (:plan anchor))
                 :open      (open-items (:plan anchor))
                 :done      (dedupe-by identity
                                       (concat (mapcat :done ps)
                                               (map (fn [[k e]] (done-line epoch at k e)) steps)))
                 :files     (dedupe-by identity
                                       (concat (mapcat :files ps)
                                               (mapcat #(files-in (:text %)) fr)))
                 :decisions (dedupe-by identity
                                       (concat (mapcat :decisions ps)
                                               (mapcat #(decisions-in (:text %))
                                                       (filter #(= "assistant" (:role %)) fr))))
                 :errors    (dedupe-by identity
                                       (concat (mapcat :errors ps)
                                               (keep (fn [[k e]] (error-line epoch k e)) steps)))
                 :citations (dedupe-by :handle
                                       (concat (filter :handle prior-citations)
                                               (mapcat :citations ps)
                                               (keep (fn [[_ e]] (citation e)) steps)))}]
    (when (or request (:task digest) (seq (:done digest)) (seq (:citations digest)))
      digest)))

;; ---------------------------------------------------------------------------
;; Budget

(defn- cap-done
  [n d]
  (let [done (:done d)
        k    (- (count done) n)]
    (if (pos? k)
      (assoc d :done (into [(str "(" k " earlier actions elided; their results stay cited under Source Coverage)")]
                           (keep-last n done)))
      d)))

(defn- caps
  [{:keys [done files decisions errors plan]}]
  (fn [d]
    (cond->> d
      done      (cap-done done)
      files     (#(update % :files (partial keep-last files)))
      decisions (#(update % :decisions (partial keep-last decisions)))
      errors    (#(update % :errors (partial keep-last errors)))
      plan      (#(update % :plan (partial keep-last plan))))))

(defn- clip-verbatim
  [n]
  (fn [d]
    (let [f (fn [s] (when s (if (> (count s) n)
                              (str (subs s 0 n) "\n[truncated to fit the digest budget]")
                              s)))]
      (-> d (update :request f) (update :task f)))))

(def shrink-steps
  "Applied cumulatively until the rendering fits. Citations are the last thing
   to lose detail and the very last to be dropped."
  [identity
   (caps {:done 40 :files 30 :decisions 10 :errors 10 :plan 30})
   (caps {:done 20 :files 15 :decisions 5 :errors 5 :plan 20})
   (comp (caps {:done 10 :files 8 :decisions 3 :errors 3})
         #(update % :citations (partial mapv short-citation)))
   (clip-verbatim 1500)
   (comp (caps {:done 3 :files 0 :decisions 0 :errors 0 :plan 10})
         #(update % :citations (partial keep-last 40)))
   (comp (clip-verbatim 400) #(update % :citations (partial keep-last 15)))])

(defn fit
  "The largest shrink of `digest` whose markdown is at most `budget-chars`,
   or nil. A nil budget is no limit."
  [digest budget-chars]
  (when digest
    (if (nil? budget-chars)
      digest
      (:fit (reduce (fn [{:keys [d] :as st} step]
                      (let [d' (step d)]
                        (if (<= (count (md/render d')) budget-chars)
                          (reduced {:fit d'})
                          (assoc st :d d'))))
                    {:d digest :fit nil}
                    shrink-steps)))))
