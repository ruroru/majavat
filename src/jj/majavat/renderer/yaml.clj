(ns jj.majavat.renderer.yaml
  (:require [clojure.string :as string]
            [jj.majavat.protocol.yaml :as protocol]
            [jj.majavat.string-builder :as sb]))

(def ^:private ^:const default-indent "  ")

(def ^:private ^:const reserved-words
  #{"y" "n" "yes" "no" "true" "false" "on" "off" "null" "none" "~"})

(def ^:private ^:const number-like #"^[-+]?\.?\d[\d_]*(\.[\d_]*)?([eE][-+]?\d+)?$")

(def ^:private ^:const indicators #{\- \? \: \, \[ \] \{ \} \# \& \* \! \| \> \' \" \% \@ \` \space \tab})

(defn- parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn- ->indent-str [indent]
  (cond
    (integer? indent) (if (pos? indent) (apply str (repeat indent " ")) default-indent)
    (keyword? indent) (->indent-str (parse-int (name indent)))
    (string? indent)  (if (= "" indent) default-indent indent)
    :else             default-indent))

(defn- key->str ^String [k]
  (cond
    (string? k)  k
    (keyword? k) (subs (str k) 1)
    :else        (str k)))

(defn- control-char? [c]
  (let [code (int c)]
    (or (< code 32) (= code 127))))

(defn- blank-char? [c]
  (or (= \space c) (= \tab c)))

(defn- unsafe-plain-char?
  "True when `s` cannot be written as a plain scalar because of what sits at
   index `i`: a control character, a `:` that ends a key, or a comment start."
  [^String s i len]
  (let [c (.charAt s i)]
    (or (control-char? c)
        (and (= \: c) (or (= (inc i) len) (blank-char? (.charAt s (inc i)))))
        (and (blank-char? c) (< (inc i) len) (= \# (.charAt s (inc i)))))))

(defn- needs-quotes? [^String s]
  (let [len (.length s)]
    (or (zero? len)
        (contains? reserved-words (string/lower-case s))
        (some? (re-matches number-like s))
        (contains? indicators (.charAt s 0))
        (blank-char? (.charAt s (dec len)))
        (loop [i 0]
          (cond
            (= i len) false
            (unsafe-plain-char? s i len) true
            :else (recur (inc i)))))))

(defn- write-quoted [sb ^String s]
  (sb/append-char sb \")
  (let [len (.length s)]
    (loop [i 0]
      (when (< i len)
        (let [c (.charAt s i)]
          (case c
            \"       (sb/append sb "\\\"")
            \\       (sb/append sb "\\\\")
            \newline (sb/append sb "\\n")
            \return  (sb/append sb "\\r")
            \tab     (sb/append sb "\\t")
            (if (control-char? c)
              (sb/append sb (format "\\u%04x" (int c)))
              (sb/append-char sb c)))
          (recur (inc i))))))
  (sb/append-char sb \"))

(defn- write-string [sb s]
  (if (needs-quotes? s)
    (write-quoted sb s)
    (sb/append sb s)))

(defn- trailing-newlines [^String s]
  (loop [i (.length s)
         n 0]
    (if (and (pos? i) (= \newline (.charAt s (dec i))))
      (recur (dec i) (inc n))
      n)))

(defn- multi-line? [^String s len]
  (loop [i 0]
    (cond
      (= i len) false
      (= \newline (.charAt s i)) true
      :else (recur (inc i)))))

(defn- first-content-line-indented?
  "True when the first non-empty line starts with a blank, which would leave the
   block's own indentation ambiguous."
  [^String s len]
  (loop [i 0]
    (cond
      (= i len) false
      (= \newline (.charAt s i)) (recur (inc i))
      :else (blank-char? (.charAt s i)))))

(defn- blank-before-break?
  "True when a line ends with a space or tab. A literal block keeps those, but
   they are the first thing an editor or a diff strips, so quote instead."
  [^String s len]
  (loop [i 0]
    (cond
      (= i len) false
      (and (blank-char? (.charAt s i))
           (or (= (inc i) len) (= \newline (.charAt s (inc i))))) true
      :else (recur (inc i)))))

(defn- unquotable-char? [c]
  (and (control-char? c)
       (not= \newline c)
       (not= \tab c)))

(defn- carries-unquotable-char? [^String s len]
  (loop [i 0]
    (cond
      (= i len) false
      (unquotable-char? (.charAt s i)) true
      :else (recur (inc i)))))

(defn- literal-safe?
  "True when `s` says the same thing as a literal block scalar: it spans lines,
   carries nothing the block form cannot hold, and ends in at most one line
   break - `|+` cannot be written unambiguously at the end of a document, so
   anything ending in more falls back to a quoted scalar."
  [^String s]
  (let [len (.length s)
        core-len (- len (trailing-newlines s))]
    (and (<= (- len core-len) 1)
         (pos? core-len)
         (multi-line? s core-len)
         (not (first-content-line-indented? s core-len))
         (not (blank-before-break? s core-len))
         (not (carries-unquotable-char? s core-len)))))

(defn- literal-block? [v]
  (and (string? v)
       (literal-safe? v)))

(defn- line-end-index
  "Index of the line break that ends the line starting at `from`, or `len` when
   that line is the last one."
  ^long [^String s ^long from ^long len]
  (loop [i from]
    (if (or (= i len) (= \newline (.charAt s i)))
      i
      (recur (inc i)))))

(defn- write-literal-block
  "Writes `s` as a literal block scalar: a `|` header, then every line indented
   by `pad`. The chomping indicator carries the trailing line break - `|` keeps
   the one the value ends with, `|-` says there is none - and empty lines are
   written bare, without padding.

   A `|` block also writes that trailing break itself. It has to: nothing can
   imply a line break that is not in the text, so a value ending in one would
   come back without it whenever the block lands at the end of the output. When
   something does follow, the extra break reads as a blank line, which clip
   chomping folds back into the same single break."
  [sb ^String s pad]
  (let [len (.length s)
        core-len (- len (trailing-newlines s))]
    (sb/append-char sb \|)
    (when (= len core-len)
      (sb/append-char sb \-))
    (loop [line-start 0]
      (when (< line-start core-len)
        (let [line-end (line-end-index s line-start core-len)]
          (sb/append sb "\n")
          (when (> line-end line-start)
            (sb/append sb pad)
            (sb/append sb (subs s line-start line-end)))
          (recur (inc line-end)))))
    (when (> len core-len)
      (sb/append sb "\n"))))

(defn- write-float [sb v]
  (let [d (double v)]
    (cond
      (Double/isNaN d)      (sb/append sb ".nan")
      (Double/isInfinite d) (sb/append sb (if (pos? d) ".inf" "-.inf"))
      :else                 (sb/append sb (str v)))))

(defn- write-scalar
  "Writes anything that fits on one line: the scalar types, plus empty
   collections, which YAML writes in flow style."
  [sb v]
  (cond
    (nil? v)     (sb/append sb "null")
    (boolean? v) (sb/append sb (if v "true" "false"))
    (string? v)  (write-string sb v)
    (ratio? v)   (sb/append sb (str (double v)))
    (float? v)   (write-float sb v)
    (number? v)  (sb/append sb (str v))
    (keyword? v) (write-string sb (key->str v))
    (symbol? v)  (write-string sb (str v))
    (map? v)     (sb/append sb "{}")
    (coll? v)    (sb/append sb "[]")
    :else        (write-string sb (str v))))

(defn- block? [v]
  (and (or (map? v) (sequential? v) (set? v))
       (some? (seq v))))

(declare write-map-entries write-seq-items)

(defn- write-entry-value
  "Writes the value of a `key:` entry: on the same line when it is a scalar,
   otherwise as an indented block on the lines below."
  [sb v pad indent]
  (cond
    (block? v)
    (let [child (str pad indent)]
      (sb/append sb "\n")
      (sb/append sb child)
      (if (map? v)
        (write-map-entries sb v child indent)
        (write-seq-items sb v child indent)))

    (literal-block? v)
    (do (sb/append-char sb \space)
        (write-literal-block sb v (str pad indent)))

    :else
    (do (sb/append-char sb \space)
        (write-scalar sb v))))

(defn- write-item-value
  "Writes the value of a `-` sequence item. A map starts on the dash line, so
   its remaining keys line up with the first one, two columns past the dash."
  [sb v pad indent]
  (cond
    (literal-block? v)
    (do (sb/append-char sb \space)
        (write-literal-block sb v (str pad indent)))

    (not (block? v))
    (do (sb/append-char sb \space)
        (write-scalar sb v))

    (map? v)
    (do (sb/append-char sb \space)
        (write-map-entries sb v (str pad "  ") indent))

    :else
    (let [child (str pad indent)]
      (sb/append sb "\n")
      (sb/append sb child)
      (write-seq-items sb v child indent))))

(defn- write-map-entries
  "Writes `m` as block mappings. The caller has already placed the cursor at
   the first entry; `pad` indents every line after it."
  [sb m pad indent]
  (loop [entries (seq m)
         first? true]
    (when entries
      (let [[k v] (first entries)]
        (when-not first?
          (sb/append sb "\n")
          (sb/append sb pad))
        (write-scalar sb k)
        (sb/append-char sb \:)
        (write-entry-value sb v pad indent)
        (recur (next entries) false)))))

(defn- write-seq-items
  "Writes `coll` as a block sequence. The caller has already placed the cursor
   at the first item; `pad` indents every line after it."
  [sb coll pad indent]
  (loop [items (seq coll)
         first? true]
    (when items
      (when-not first?
        (sb/append sb "\n")
        (sb/append sb pad))
      (sb/append-char sb \-)
      (write-item-value sb (first items) pad indent)
      (recur (next items) false))))

(defrecord DefaultYamlSerializer []
  protocol/Yaml
  (to-yaml [_ value opts]
    (let [indent (->indent-str (:indent opts))
          sb (sb/create-string-builder)]
      (cond
        (literal-block? value) (write-literal-block sb value indent)
        (not (block? value))   (write-scalar sb value)
        (map? value)           (write-map-entries sb value "" indent)
        :else                  (write-seq-items sb value "" indent))
      (sb/build sb))))
