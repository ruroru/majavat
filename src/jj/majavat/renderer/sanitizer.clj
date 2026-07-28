(ns jj.majavat.renderer.sanitizer
  (:require [jj.majavat.protocol.renderer.sanitizer :as sanitizer]
            [jj.majavat.string-builder :as sb]))

(def ^:private hex-digits "0123456789abcdef")

(defn- unicode-escape
  [code]
  (str "\\u"
       (nth hex-digits (bit-and (bit-shift-right code 12) 0xf))
       (nth hex-digits (bit-and (bit-shift-right code 8) 0xf))
       (nth hex-digits (bit-and (bit-shift-right code 4) 0xf))
       (nth hex-digits (bit-and code 0xf))))

(defn- needs-json-escaping? [^String s len]
  (loop [i 0]
    (if (< i len)
      (let [c (.charAt s i)]
        (if (or (= c \<) (= c \>) (= c \&)
                (= c \") (= c \'))
          true
          (recur (inc i))))
      false)))

(defn- needs-html-escaping? [^String s len]
  (loop [i 0]
    (if (< i len)
      (let [c (.charAt s i)]
        (if (or (= c \&) (= c \<) (= c \>) (= c \") (= c \'))
          true
          (recur (inc i))))
      false)))

(defn- escape-html-sb [^String s len sb]
  (loop [i 0]
    (if (< i len)
      (let [c (.charAt ^String s i)]
        (case c
          \& (sb/append sb "&amp;")
          \< (sb/append sb "&lt;")
          \> (sb/append sb "&gt;")
          \" (sb/append sb "&quot;")
          \' (sb/append sb "&apos;")
          (sb/append sb c))
        (recur (inc i)))
      (sb/build sb))))

(defn- escape-html [s len]
  (escape-html-sb s len (sb/create-string-builder (* len 2))))

(defrecord Html []
  sanitizer/Sanitizer
  (sanitize [_ s]
    (when s
      (if (needs-html-escaping? s (count s))
        (escape-html s (count s))
        s))))

(defn- escape-json-sb [^String s len sb]
  (loop [i 0]
    (if (< i len)
      (let [c (.charAt ^String s i)]
        (case c
          \" (sb/append sb "\\\"")
          \\ (sb/append sb "\\\\")
          \/ (sb/append sb "\\/")
          \backspace (sb/append sb "\\b")
          \formfeed (sb/append sb "\\f")
          \newline (sb/append sb "\\n")
          \return (sb/append sb "\\r")
          \tab (sb/append sb "\\t")
          (if (< (int c) 32)
            (sb/append sb (unicode-escape (int c)))
            (sb/append sb c)))
        (recur (inc i)))
      (sb/build sb))))

(defn- escape-json [s len]
  (escape-json-sb s len (sb/create-string-builder (* len 2))))

(defrecord Json []
  sanitizer/Sanitizer
  (sanitize [_ s]
    (when s
      (if (needs-json-escaping? s (count s))
        (escape-json s (count s))
        s))))

(defrecord None []
  sanitizer/Sanitizer
  (sanitize [_ s] s))
