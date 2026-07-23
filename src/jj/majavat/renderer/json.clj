(ns jj.majavat.renderer.json
  (:require [jj.majavat.protocol.json :as protocol]))

(declare ^:private write-value)

(defn- parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn- append-string [^StringBuilder sb ^String s]
  (.append sb \")
  (let [len (.length s)]
    (loop [i 0]
      (when (< i len)
        (let [c (.charAt s i)]
          (case c
            \"         (.append sb "\\\"")
            \\         (.append sb "\\\\")
            \backspace (.append sb "\\b")
            \formfeed  (.append sb "\\f")
            \newline   (.append sb "\\n")
            \return    (.append sb "\\r")
            \tab       (.append sb "\\t")
            (if (< (int c) 32)
              (.append sb (format "\\u%04x" (int c)))
              (.append sb c)))
          (recur (inc i))))))
  (.append sb \"))

(defn- key->str ^String [k]
  (cond
    (string? k)  k
    (keyword? k) (subs (str k) 1)
    :else        (str k)))

(defn- newline-indent [^StringBuilder sb indent level]
  (when indent
    (.append sb "\n")
    (dotimes [_ level]
      (.append sb ^String indent))))

(defn- write-object [^StringBuilder sb m indent level]
  (if (empty? m)
    (.append sb "{}")
    (let [inner (inc level)]
      (.append sb "{")
      (loop [es (seq m) first? true]
        (when es
          (let [[k v] (first es)]
            (when-not first? (.append sb ","))
            (newline-indent sb indent inner)
            (append-string sb (key->str k))
            (.append sb (if indent ": " ":"))
            (write-value sb v indent inner)
            (recur (next es) false))))
      (newline-indent sb indent level)
      (.append sb "}"))))

(defn- write-array [^StringBuilder sb coll indent level]
  (if (empty? coll)
    (.append sb "[]")
    (let [inner (inc level)]
      (.append sb "[")
      (loop [xs (seq coll) first? true]
        (when xs
          (when-not first? (.append sb ","))
          (newline-indent sb indent inner)
          (write-value sb (first xs) indent inner)
          (recur (next xs) false)))
      (newline-indent sb indent level)
      (.append sb "]"))))

(defn- write-value [^StringBuilder sb v indent level]
  (cond
    (nil? v)     (.append sb "null")
    (boolean? v) (.append sb (if v "true" "false"))
    (string? v)  (append-string sb v)
    (ratio? v)   (.append sb (str (double v)))
    (and (float? v)
         (let [d (double v)]
           (or (Double/isNaN d) (Double/isInfinite d))))
    (.append sb "null")
    (number? v)  (.append sb (str v))
    (keyword? v) (append-string sb (key->str v))
    (symbol? v)  (append-string sb (str v))
    (map? v)     (write-object sb v indent level)
    (or (sequential? v) (set? v))
    (write-array sb v indent level)
    :else        (append-string sb (str v))))

(defn- ->indent-str [indent]
  (cond
    (nil? indent)     nil
    (integer? indent) (apply str (repeat indent " "))
    (keyword? indent) (when-let [n (parse-int (name indent))]
                        (apply str (repeat n " ")))
    (string? indent)  indent
    :else             nil))

(defn- write
  "Serializes v to a JSON string. When indent is a positive integer (or a
  keyword/string parsed as one) the output is pretty-printed with that many
  spaces per level; otherwise it is compact."
  ([v] (write v nil))
  ([v indent]
   (let [sb (StringBuilder.)]
     (write-value sb v (->indent-str indent) 0)
     (.toString sb))))

(defrecord DefaultJsonSerializer []
  protocol/Json
  (to-json [_ value opts]
    (write value (:indent opts))))
