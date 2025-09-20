(ns jj.majavat.renderer.sanitizer)

(defprotocol Sanitizer
  (sanitize [_ input]))

(defrecord Html []
  Sanitizer
  (sanitize [_ s]
    (if (nil? s)
      s
      (let [len (.length ^String s)
            sb (StringBuilder. (int (* len 1.2)))]
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
              (recur (unchecked-inc i)))
            (.toString sb)))))))

(defrecord Json []
  Sanitizer
  (sanitize [_ s]
    (if (nil? s)
      s
      (let [len (.length ^String s)
            sb (StringBuilder. (int (* len 1.2)))]
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
              (recur (unchecked-inc i)))
            (.toString sb)))))))