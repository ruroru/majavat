(ns jj.majavat.renderer.filters
  (:require [clojure.string :as str]
            [clojure.tools.logging :as logger])
  (:import (java.time LocalDate)
           (java.time.format DateTimeFormatter)))

(def formatter-cache (atom {}))

(defn title-case [s]
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
        (.toString sb)))))

(defn upper-roman [v]
  [v]
  (when v
    (let [roman-pattern #"(?i)\b(?=[mdclxvi])M{0,4}(?:CM|CD|D?C{0,3})(?:XC|XL|L?X{0,3})(?:IX|IV|V?I{0,3})\b"]
      (str/replace v roman-pattern str/upper-case))))

(defn file-size
  [bytes]
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
              (str (format "%.1f" pb) " PB"))))))))

(defn as-long [s]
  (try
    (Long/parseLong s)
    (catch Exception _ nil)))

(defn as-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn get-default [v filter-args]
  (if (some? v)
    v
    (first filter-args)))

(defn- get-formatter [pattern]
  (try
    (or (get @formatter-cache pattern)
        (let [formatter (DateTimeFormatter/ofPattern pattern)]
          (swap! formatter-cache assoc pattern formatter)
          formatter))
    (catch Exception _
      nil)))

(defn ->formatted-local-date [v filter-args]
  (let [pattern (first filter-args)
        date-time-formatter (get-formatter pattern)]
    (if (some? date-time-formatter)
      (.format ^LocalDate v date-time-formatter)
      (do
        (logger/errorf "%s is not a valid pattern." pattern)
        (str v)))))
