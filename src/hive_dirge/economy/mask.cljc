(ns hive-dirge.economy.mask
  "Observation masking (The Complexity Trap, arXiv 2508.21433): every tool
   result older than the last N turns becomes a one-line stub naming the
   tool, an args digest and the Handle context_retrieve answers. The action
   record (user and assistant messages, every tool call) is kept whole, no
   message is dropped and none is reordered. Pure: data in, data out.

   Messages arrive in dirge's transcript shape (keys as keywords):
     {:role \"user\" :content \"text\"}
     {:role \"assistant\" :content [{:type \"toolCall\" :id :name :arguments} ...]}
     {:role \"toolResult\" :toolCallId :toolName :content [{:type \"text\" :text}] ...}
   A turn is one assistant message; a message's age is the number of
   assistant messages after it."
  (:require [clojure.string :as str]
            [hive-dirge.economy.domain :as d]
            [hive-dirge.economy.ports :as p]))

(def stub-prefix "[masked: ")

(def stub-args-max
  "Chars of the args digest a stub carries."
  80)

(def min-masked-chars
  "Results this short are kept: their stub would save nothing."
  200)

;; ---------------------------------------------------------------------------
;; Reading messages

(defn tool-result?
  [m]
  (= "toolResult" (:role m)))

(defn assistant?
  [m]
  (= "assistant" (:role m)))

(defn content-text
  "The text of a message's :content: a string as is, the text blocks of a
   block vector joined by newlines, else \"\"."
  [content]
  (cond
    (string? content)     content
    (sequential? content) (str/join "\n" (keep (fn [b]
                                                 (when (and (map? b)
                                                            (= "text" (:type b))
                                                            (string? (:text b)))
                                                   (:text b)))
                                               content))
    :else                 ""))

(defn stub?
  "True for a tool result already masked."
  [m]
  (str/starts-with? (content-text (:content m)) stub-prefix))

(defn ages
  "Per message, how many assistant messages follow it (0: the current turn)."
  [messages]
  (loop [ms (reverse messages) seen 0 out ()]
    (if (empty? ms)
      (vec out)
      (let [m (first ms)]
        (recur (rest ms) (if (assistant? m) (inc seen) seen) (conj out seen))))))

(defn calls-by-id
  "{tool-call-id {:tool :args}} of every toolCall block in `messages`."
  [messages]
  (into {}
        (for [m messages
              :when (and (assistant? m) (sequential? (:content m)))
              b (:content m)
              :when (and (map? b) (= "toolCall" (:type b)))]
          [(str (:id b)) {:tool (:name b) :args (:arguments b)}])))

(defn args-digest
  "One line of at most stub-args-max chars naming the call's args, or nil."
  [args]
  (when-let [s (d/args-text args)]
    (if (> (count s) stub-args-max) (str (subs s 0 stub-args-max) "…") s)))

;; ---------------------------------------------------------------------------
;; Promote: what to mask

(defn- keep-turns?
  [n]
  (and (integer? n) (pos? n)))

(defn maskable
  "Tool results at least `keep-turns` turns old, not yet stubs, longer than
   min-masked-chars: [{:index :tool-use-id :tool :args :text}]. Empty when
   `keep-turns` is not a positive integer (masking off)."
  [keep-turns messages]
  (if-not (keep-turns? keep-turns)
    []
    (let [ms    (vec messages)
          ag    (ages ms)
          calls (calls-by-id ms)]
      (vec (keep-indexed
            (fn [i m]
              (when (and (tool-result? m) (>= (nth ag i) keep-turns) (not (stub? m)))
                (let [text (content-text (:content m))
                      id   (some-> (:toolCallId m) str)]
                  (when (> (count text) min-masked-chars)
                    {:index       i
                     :tool-use-id id
                     :tool        (str (or (:toolName m) (:tool (get calls id)) "tool"))
                     :args        (:args (get calls id))
                     :text        text}))))
            ms)))))

;; ---------------------------------------------------------------------------
;; Stub

(defn stub-text
  [tool args handle]
  (str stub-prefix tool
       (when-let [a (args-digest args)] (str " " a))
       " " (d/cite handle)
       "; context_retrieve recovers it]"))

(defn stub-message
  "`m` with its content replaced by `text`; every other key is kept."
  [m text]
  (assoc m :content [{:type "text" :text text}]))

(defn mask-messages
  "`messages` with each `maskable` result replaced by its stub. `handles`
   is {index handle}; a result without a handle is kept whole, so every stub
   is recoverable. Same count, same order; idempotent."
  [{:keys [keep-turns handles]} messages]
  (let [ms (vec messages)]
    (reduce (fn [acc {:keys [index tool args]}]
              (if-let [h (get handles index)]
                (update acc index stub-message (stub-text tool args h))
                acc))
            ms
            (maskable keep-turns ms))))

;; ---------------------------------------------------------------------------
;; Strategy

(defrecord HiveDirgeEconomyMaskShaper [keep-turns]
  p/IContextShaper
  (wanted [_ messages] (maskable keep-turns messages))
  (shape [_ messages handles]
    (mask-messages {:keep-turns keep-turns :handles handles} messages)))

(def config-key :economy/mask-after-turns)

(defn make-shaper
  "A MaskShaper keeping the last `:economy/mask-after-turns` turns whole, or
   nil when the key is not a positive integer (default: off)."
  [config]
  (let [n (get config config-key)]
    (when (keep-turns? n)
      (->HiveDirgeEconomyMaskShaper n))))
