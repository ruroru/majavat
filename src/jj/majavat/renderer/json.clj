(ns jj.majavat.renderer.json
  (:require [jj.majavat.protocol.json :as protocol]))

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

(defn- ->indent-str [indent]
  (cond
    (nil? indent)     nil
    (integer? indent) (apply str (repeat indent " "))
    (keyword? indent) (when-let [n (parse-int (name indent))]
                        (apply str (repeat n " ")))
    (string? indent)  indent
    :else             nil))

(defn- value-type [v]
  (cond
    (nil? v)                      :null
    (boolean? v)                  :boolean
    (string? v)                   :string
    (ratio? v)                    :ratio
    (float? v)                    :float
    (number? v)                   :number
    (keyword? v)                  :keyword
    (symbol? v)                   :symbol
    (map? v)                      :object
    (or (sequential? v) (set? v)) :array
    :else                         :default))

(defmulti ^:private write-value (fn [_sb v _indent _level] (value-type v)))

(defmethod write-value :null [^StringBuilder sb _ _ _]
  (.append sb "null"))

(defmethod write-value :boolean [^StringBuilder sb v _ _]
  (.append sb (if v "true" "false")))

(defmethod write-value :string [^StringBuilder sb v _ _]
  (append-string sb v))

(defmethod write-value :ratio [^StringBuilder sb v _ _]
  (.append sb (str (double v))))

(defmethod write-value :float [^StringBuilder sb v _ _]
  (let [d (double v)]
    (if (or (Double/isNaN d) (Double/isInfinite d))
      (.append sb "null")
      (.append sb (str v)))))

(defmethod write-value :number [^StringBuilder sb v _ _]
  (.append sb (str v)))

(defmethod write-value :keyword [^StringBuilder sb v _ _]
  (append-string sb (key->str v)))

(defmethod write-value :symbol [^StringBuilder sb v _ _]
  (append-string sb (str v)))

(defmethod write-value :object [^StringBuilder sb m indent level]
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

(defmethod write-value :array [^StringBuilder sb coll indent level]
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

(defmethod write-value :default [^StringBuilder sb v _ _]
  (append-string sb (str v)))

(defrecord DefaultJsonSerializer []
  protocol/Json
  (to-json [_ value opts]
    (let [sb (StringBuilder.)]
      (write-value sb value (->indent-str (:indent opts)) 0)
      (.toString sb))
    ))
