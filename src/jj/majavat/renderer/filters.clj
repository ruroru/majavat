(ns jj.majavat.renderer.filters
  (:require [clojure.string :as str]
            [clojure.tools.logging :as logger])
  (:import (java.net URI URL)
           (java.time Instant LocalDate LocalDateTime LocalTime ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (java.util UUID)))

(def ^:private formatter-cache (atom {}))
(def ^:const ^:private roman-regex #"(?i)\b(?=[mdclxvi])M{0,4}(?:CM|CD|D?C{0,3})(?:XC|XL|L?X{0,3})(?:IX|IV|V?I{0,3})\b")

(defn title-case [s args]
  (when (some? s)
    (let [sb (StringBuilder.)
          char-array (char-array s)]
      (loop [index 0 new-word? true]
        (if (< index (count char-array))
          (let [c (aget char-array index)]
            (if (or (Character/isWhitespace c) (= c \-))
              (do
                (.append sb c)
                (recur (inc index) true))
              (do
                (if new-word?
                  (.append sb (Character/toUpperCase c))
                  (.append sb (Character/toLowerCase c)))
                (recur (inc index) false))))
          (.toString sb))))))

(defn upper-roman [v args]
  (when v (str/replace v roman-regex str/upper-case)))

(defn file-size
  [bytes args]
  (when (number? bytes)
    (when bytes
      (when (number? bytes)
        (let [abs-bytes (Math/abs (double bytes))]
          (cond
            (< abs-bytes 1024)
            (if (== bytes 1)
              "1 byte"
              (str (int bytes) " bytes"))

            (< abs-bytes (* 1024 1024))
            (let [kb (/ bytes 1024.0)]
              (if (== (mod kb 1) 0)
                (str (int kb) " KB")
                (str (format "%.1f" kb) " KB")))

            (< abs-bytes (* 1024 1024 1024))
            (let [mb (/ bytes (* 1024.0 1024))]
              (if (== (mod mb 1) 0)
                (str (int mb) " MB")
                (str (format "%.1f" mb) " MB")))

            (< abs-bytes (* 1024 1024 1024 1024))
            (let [gb (/ bytes (* 1024.0 1024 1024))]
              (if (== (mod gb 1) 0)
                (str (int gb) " GB")
                (str (format "%.1f" gb) " GB")))

            (< abs-bytes (* 1024 1024 1024 1024 1024))
            (let [tb (/ bytes (* 1024.0 1024 1024 1024))]
              (if (== (mod tb 1) 0)
                (str (int tb) " TB")
                (str (format "%.1f" tb) " TB")))

            :else
            (let [pb (/ bytes (* 1024.0 1024 1024 1024 1024))]
              (if (== (mod pb 1) 0)
                (str (int pb) " PB")
                (str (format "%.1f" pb) " PB")))))))))

(defn as-long [s args]
  (try
    (Long/parseLong s)
    (catch Exception _ nil)))

(defn as-int [s args]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn round-number [n args]
  (when (number? n)
    (Math/round (double n))))

(defn get-floor [n args]
  (when (number? n)
    (Math/floor (double n))))

(defn get-ceiling [n args]
  (when (number? n)
    (Math/ceil (double n))))

(defn get-absolute-value [n args]
  (when (number? n)
    (Math/abs (double n))))

(defn append [v filter-args]
  (when (some? v)
    (let [arg (first filter-args)]
      (str v arg))))

(defn prepend [v filter-args]
  (when (some? v)
    (let [arg (first filter-args)]
      (str arg v))))


(defn get-default [v filter-args]
  (if (some? v)
    v
    (first filter-args)))

(defn- get-date-time-formatter [pattern]
  (try
    (or (get @formatter-cache pattern)
        (let [formatter (DateTimeFormatter/ofPattern pattern)]
          (swap! formatter-cache assoc pattern formatter)
          formatter))
    (catch Exception _
      nil)))

(defn ->formatted-local-date [v filter-args]
  (let [pattern (first filter-args)
        date-time-formatter (get-date-time-formatter pattern)]
    (if (some? date-time-formatter)
      (.format ^LocalDate v date-time-formatter)
      (do
        (logger/errorf "%s is not a valid pattern." pattern)
        (str v)))))

(defn ->formatted-local-date-time [v filter-args]
  (let [pattern (first filter-args)
        date-time-formatter (get-date-time-formatter pattern)]
    (if (some? date-time-formatter)
      (.format ^LocalDateTime v date-time-formatter)
      (do
        (logger/errorf "%s is not a valid pattern." pattern)
        (str v)))))

(defn ->formatted-local-time [v filter-args]
  (let [pattern (first filter-args)
        date-time-formatter (get-date-time-formatter pattern)]
    (if (some? date-time-formatter)
      (.format ^LocalTime v date-time-formatter)
      (do
        (logger/errorf "%s is not a valid pattern." pattern)
        (str v)))))

(defn- string->time-zone [zone]
  (try
    (ZoneId/of zone)
    (catch Exception _
      nil)))


(defn ->uuid-as-string [^UUID uuid]
  (.toString uuid))

(defn ->formatted-zoned-date-time [v filter-args]
  (let [pattern (first filter-args)
        time-zone (string->time-zone (second filter-args))
        date-time-formatter (get-date-time-formatter pattern)]
    (if (some? date-time-formatter)
      (if (some? time-zone)
        (.format (.withZoneSameInstant ^ZonedDateTime v ^ZoneId time-zone) date-time-formatter)
        (.format ^ZonedDateTime v date-time-formatter))
      (do
        (logger/errorf "%s is not a valid pattern." pattern)
        (str v)))))



(defn ->formatted-instant [v filter-args]
  (let [pattern (first filter-args)
        time-zone (string->time-zone (second filter-args))
        date-time-formatter (get-date-time-formatter pattern)]
    (if (some? date-time-formatter)
      (if (some? time-zone)
        (.format (.atZone ^Instant v ^ZoneId time-zone) date-time-formatter)
        (.format (.atZone ^Instant v (ZoneId/systemDefault)) date-time-formatter))
      (do
        (logger/errorf "%s is not a valid pattern." pattern)
        (str v)))))

(defn ->url-to-string [^URL url]
  (.toString url))

(defn ->uri-to-string [^URI uri]
  (.toString uri))

(defn ->handle-where [v filter-args]
  (when (sequential? v)
    (into []
          (let [[filter-key filter-value] filter-args]
            (filter #(= (get % filter-key) filter-value) v)))))

(defn seq->str [v]
  (str v))

(defn slugify [v args]
  (when v
    (-> (str v)
        clojure.string/lower-case
        (clojure.string/replace #"[^\w\s-]" "")
        (clojure.string/replace #"\s+" "-")
        (clojure.string/replace #"-+" "-")
        clojure.string/trim)))

(defn trim-string [value args]
  (when (some? value)
    (.trim ^String value)))

(defn- should-upper-case-case? [^String k len]
  (loop [i 0]
    (if (< i len)
      (if (Character/isUpperCase (.charAt k i))
        (recur (inc i))
        false)
      true)))

(defn upper-case-string [^String value _]
  (when (some? value)
    (if-not (should-upper-case-case? value (.length value))
      (.toUpperCase ^String value)
      value)))

(defn- should-lower-case? [^String k len]
  (loop [i 0]
    (if (< i len)
      (if (Character/isLowerCase (.charAt k i))
        (recur (inc i))
        false)
      true)))

(defn lower-case-string [^String k _]
  (when (some? k)
    (if-not (should-lower-case? k (.length k))
      (.toLowerCase ^String k)
      k)))

(defn- should-capitalize? [^String value len]
  (loop [i 0]
    (cond
      (= i len) true
      (= i 0) (if (Character/isUpperCase (.charAt value i))
                (recur (inc i))
                false)
      :else (if (Character/isLowerCase (.charAt value i))
              (recur (inc i))
              false))))

(defn capitalize-string [^String value _]
  (when (some? value)
    (if-not (should-capitalize? value (.length value))
      (clojure.string/capitalize ^String value)
      value)))

(defn get-name [value args]
  (if (keyword? value)
    (name value)
    value))

(defn dec-number [n args]
  (when (number? n)
    (dec n)))

(defn inc-number [n args]
  (when (number? n)
    (inc n)))

(defn handle-date [% args]
  (cond
    (instance? LocalDate %) (->formatted-local-date % args)
    (instance? LocalDateTime %) (->formatted-local-date-time % args)
    (instance? LocalTime %) (->formatted-local-time % args)
    (instance? ZonedDateTime %) (->formatted-zoned-date-time % args)
    (instance? Instant %) (->formatted-instant % args)
    :else (str %)))

(defn handle-str [v args]
  (if (sequential? v) (seq->str v) (str v)))

(defn get-first [v ars]
  (when (or
          (sequential? v)
          (map? v))
    (first v)))

(defn get-rest [v ars]

  (cond
    (sequential? v) (rest v)
    (map? v) (into {} (rest v))
    :else nil
    ))