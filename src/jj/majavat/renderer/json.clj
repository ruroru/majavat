(ns jj.majavat.renderer.json
  (:require [jj.majavat.protocol.json :as protocol]
            [jj.majavat.string-builder :as sb]))

(defn- parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn- append-string [sb ^String s]
  (sb/append sb \")
  (let [len (.length s)]
    (loop [i 0]
      (when (< i len)
        (let [c (.charAt s i)]
          (case c
            \"         (sb/append sb "\\\"")
            \\         (sb/append sb "\\\\")
            \backspace (sb/append sb "\\b")
            \formfeed  (sb/append sb "\\f")
            \newline   (sb/append sb "\\n")
            \return    (sb/append sb "\\r")
            \tab       (sb/append sb "\\t")
            (if (< (int c) 32)
              (sb/append sb (format "\\u%04x" (int c)))
              (sb/append sb c)))
          (recur (inc i))))))
  (sb/append sb \"))

(defn- key->str ^String [k]
  (cond
    (string? k)  k
    (keyword? k) (subs (str k) 1)
    :else        (str k)))

(defn- newline-indent [sb indent level]
  (when indent
    (sb/append sb "\n")
    (dotimes [_ level]
      (sb/append sb ^String indent))))

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

(defmethod write-value :null [sb _ _ _]
  (sb/append sb "null"))

(defmethod write-value :boolean [sb v _ _]
  (sb/append sb (if v "true" "false")))

(defmethod write-value :string [sb v _ _]
  (append-string sb v))

(defmethod write-value :ratio [sb v _ _]
  (sb/append sb (str (double v))))

(defmethod write-value :float [sb v _ _]
  (let [d (double v)]
    (if (or (Double/isNaN d) (Double/isInfinite d))
      (sb/append sb "null")
      (sb/append sb (str v)))))

(defmethod write-value :number [sb v _ _]
  (sb/append sb (str v)))

(defmethod write-value :keyword [sb v _ _]
  (append-string sb (key->str v)))

(defmethod write-value :symbol [sb v _ _]
  (append-string sb (str v)))

(defmethod write-value :object [sb m indent level]
  (if (empty? m)
    (sb/append sb "{}")
    (let [inner (inc level)]
      (sb/append sb "{")
      (loop [es (seq m) first? true]
        (when es
          (let [[k v] (first es)]
            (when-not first? (sb/append sb ","))
            (newline-indent sb indent inner)
            (append-string sb (key->str k))
            (sb/append sb (if indent ": " ":"))
            (write-value sb v indent inner)
            (recur (next es) false))))
      (newline-indent sb indent level)
      (sb/append sb "}"))))

(defmethod write-value :array [sb coll indent level]
  (if (empty? coll)
    (sb/append sb "[]")
    (let [inner (inc level)]
      (sb/append sb "[")
      (loop [xs (seq coll) first? true]
        (when xs
          (when-not first? (sb/append sb ","))
          (newline-indent sb indent inner)
          (write-value sb (first xs) indent inner)
          (recur (next xs) false)))
      (newline-indent sb indent level)
      (sb/append sb "]"))))

(defmethod write-value :default [sb v _ _]
  (append-string sb (str v)))

(defrecord DefaultJsonSerializer []
  protocol/Json
  (to-json [_ value opts]
    (let [sb (sb/create-string-builder)]
      (write-value sb value (->indent-str (:indent opts)) 0)
      (sb/build sb))))
