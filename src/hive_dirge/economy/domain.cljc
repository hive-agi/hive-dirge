(ns hive-dirge.economy.domain
  "Context-economy values: Observation {:tool :signature :body :tokens :error?},
   Handle (short lowercase hex content address), and range slicing. Pure."
  (:require [clojure.string :as str]))

(def handle-min-len 8)
(def handle-max-len 16)

;; ---------------------------------------------------------------------------
;; Canonical text

(declare canonical)

(defn- canonical-entries
  [m]
  (->> m
       (map (fn [[k v]] [(canonical k) (canonical v)]))
       (sort-by first)
       (map (fn [[k v]] (str k " " v)))))

(defn canonical
  "Printer-independent text of `v`: map keys and set members sorted."
  [v]
  (cond
    (map? v)        (str "{" (str/join ", " (canonical-entries v)) "}")
    (set? v)        (str "#{" (str/join " " (sort (map canonical v))) "}")
    (vector? v)     (str "[" (str/join " " (map canonical v)) "]")
    (sequential? v) (str "(" (str/join " " (map canonical v)) ")")
    :else           (pr-str v)))

(defn signature
  "Tool name + canonical args."
  [tool args]
  (str tool " " (canonical (or args {}))))

(defn body-text
  "A tool result as text; non-string results are printed canonically."
  [result]
  (cond
    (string? result) result
    (nil? result)    ""
    :else            (canonical result)))

(defn estimate-tokens
  "chars/4, rounded up."
  [text]
  (quot (+ (count text) 3) 4))

;; ---------------------------------------------------------------------------
;; Observation

(defn observation
  "Promotes one after-tool-call context to an Observation."
  [{:keys [tool args result error?]}]
  (let [body (body-text result)]
    {:tool      (str tool)
     :signature (signature tool args)
     :body      body
     :tokens    (estimate-tokens body)
     :error?    (boolean error?)}))

;; ---------------------------------------------------------------------------
;; Handle

(def ^:private mask32 0xffffffff)
(def ^:private fnv-prime 16777619)
(def ^:private fnv-offset 2166136261)

(defn- fnv1a32
  [seed text]
  (reduce (fn [h c] (bit-and (* (bit-xor h (int c)) fnv-prime) mask32))
          seed
          text))

(defn- hex8
  [n]
  (apply str (map (fn [shift] (nth "0123456789abcdef" (bit-and (bit-shift-right n shift) 15)))
                  [28 24 20 16 12 8 4 0])))

(defn content-key
  "Full 16-hex content address of signature + body."
  [{:keys [signature body]}]
  (let [text (str signature "\u0000" body)
        h1   (fnv1a32 fnv-offset text)
        h2   (fnv1a32 (bit-xor h1 0x9e3779b9) text)]
    (str (hex8 h1) (hex8 h2))))

(defn mint-handle
  "Shortest prefix (>= 8 hex) of `full` not held by a different content key.
   `owner-of` answers the full key holding a handle, or nil."
  [owner-of full]
  (or (some (fn [n]
              (let [h (subs full 0 n)
                    o (owner-of h)]
                (when (or (nil? o) (= o full)) h)))
            (range handle-min-len (inc handle-max-len) 2))
      (loop [i 1]
        (let [h (str full "-" i)
              o (owner-of h)]
          (if (or (nil? o) (= o full)) h (recur (inc i)))))))

(defn parse-handle
  "Normalises user text (optional leading §, any case) to a handle, or nil."
  [s]
  (when (string? s)
    (let [t (str/lower-case (str/trim s))
          t (if (str/starts-with? t "§") (subs t 1) t)]
      (when (re-matches #"[0-9a-f]{8,16}(-[0-9]+)?" t) t))))

(defn cite
  [handle]
  (str "§" handle))

;; ---------------------------------------------------------------------------
;; Range

(defn- ->int
  [x]
  (cond
    (integer? x) x
    (string? x)  (parse-long (str/trim x))
    :else        nil))

(defn parse-range
  "{:unit :start :end} from tool params, or nil for the whole body.
   :unit is :lines (default) or :chars; bounds are 1-based inclusive."
  [{:keys [unit start end]}]
  (let [s (->int start)
        e (->int end)]
    (when (or s e)
      {:unit  (if (= "chars" (some-> unit name)) :chars :lines)
       :start (max 1 (or s 1))
       :end   e})))

(defn slice
  "The part of `body` inside `rng` (see parse-range); whole body when nil."
  [rng body]
  (if (nil? rng)
    body
    (let [{:keys [unit start end]} rng]
      (case unit
        :chars (let [n (count body)
                     a (min n (dec start))
                     b (min n (max a (or end n)))]
                 (subs body a b))
        (let [ls (str/split body #"\n" -1)
              n  (count ls)
              a  (min n (dec start))
              b  (min n (max a (or end n)))]
          (str/join "\n" (subvec ls a b)))))))
