(ns jj.majavat.renderer.sanitizer)

(def ^:private ^:const escape-growth-factor 1.5)

(defprotocol Sanitizer
  (sanitize [_ input]))

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

(defn- escape-html-sb [^String s len ^StringBuilder sb]
  (loop [i 0]
    (if (< i len)
      (let [c (.charAt ^String s i)]
        (case c
          \& (.append sb "&amp;")
          \< (.append sb "&lt;")
          \> (.append sb "&gt;")
          \" (.append sb "&quot;")
          \' (.append sb "&apos;")
          (.append sb c))
        (recur (Integer/sum i 1)))
      (.toString sb))))

(defn- escape-html [s len]
  (escape-html-sb s len (StringBuilder. (int (* len escape-growth-factor)))))

(defrecord Html []
  Sanitizer
  (sanitize [_ s]
    (when s
      (if (needs-html-escaping? s (.length ^String s))
        (escape-html s (.length ^String s))
        s))))

(defn- escape-json-sb [^String s len ^StringBuilder sb]
  (loop [i 0]
    (if (< i len)
      (let [c (.charAt ^String s i)]
        (case c
          \" (.append sb "\\\"")
          \\ (.append sb "\\\\")
          \/ (.append sb "\\/")
          \backspace (.append sb "\\b")
          \formfeed (.append sb "\\f")
          \newline (.append sb "\\n")
          \return (.append sb "\\r")
          \tab (.append sb "\\t")
          (if (< (int c) 32)
            (.append sb (format "\\u%04x" (int c)))
            (.append sb c)))
        (recur (Integer/sum i 1)))
      (.toString ^StringBuilder sb))))

(defn- escape-json [s len]
  (escape-json-sb s len (StringBuilder. (int (* len escape-growth-factor)))))

(defrecord Json []
  Sanitizer
  (sanitize [_ s]
    (when s
      (if (needs-json-escaping? s (.length ^String s))
        (escape-json s (.length ^String s))
        s))))