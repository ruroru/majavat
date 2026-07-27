(ns jj.majavat.renderer.sanitizer
  (:require [jj.majavat.protocol.renderer.sanitizer :as sanitizer]
            [jj.majavat.string-builder :as sb]))

(defn- needs-json-escaping? [^String s len]
  (loop [i 0]
    (if (< i len)
      (let [c (.charAt s i)]
        (if (or (= c \<) (= c \>) (= c \&)
                (= c \") (= c \'))
          true
          (recur (Integer/sum i 1))))
      false)))

(defn- needs-html-escaping? [^String s len]
  (loop [i 0]
    (if (< i len)
      (let [c (.charAt s i)]
        (if (or (= c \&) (= c \<) (= c \>) (= c \") (= c \'))
          true
          (recur (Integer/sum i 1))))
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
        (recur (Integer/sum i 1)))
      (sb/build sb))))

(defn- escape-html [s len]
  (escape-html-sb s len (sb/create-string-builder (^[int int] Math/multiplyExact len 2))))

(defrecord Html []
  sanitizer/Sanitizer
  (sanitize [_ s]
    (when s
      (if (needs-html-escaping? s (.length ^String s))
        (escape-html s (.length ^String s))
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
            (sb/append sb (format "\\u%04x" (int c)))
            (sb/append sb c)))
        (recur (Integer/sum i 1)))
      (sb/build sb))))

(defn- escape-json [s len]
  (escape-json-sb s len (sb/create-string-builder (^[int int] Math/multiplyExact len 2))))

(defrecord Json []
  sanitizer/Sanitizer
  (sanitize [_ s]
    (when s
      (if (needs-json-escaping? s (.length ^String s))
        (escape-json s (.length ^String s))
        s))))

(defrecord None []
  sanitizer/Sanitizer
  (sanitize [_ s] s))