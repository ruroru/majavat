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

(defn- html-escaped-length
  "Exact length of the HTML-escaped form of `s`, or -1 when no character needs
   escaping (so the caller returns the input unchanged, allocating nothing).
   The per-char additions match escape-html-sb's replacements exactly:
   & -> &amp; (+4), < -> &lt; / > -> &gt; (+3), \" -> &quot; / ' -> &apos; (+5)."
  [^String s len]
  (loop [i 0 extra 0]
    (if (< i len)
      (recur (inc i)
             (+ extra (long (case (.charAt s i)
                              \& 4
                              (\< \>) 3
                              (\" \') 5
                              0))))
      (if (zero? extra) -1 (+ len extra)))))

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

(defrecord Html []
  sanitizer/Sanitizer
  (sanitize [_ s]
    (when s
      (let [len (count s)
            out-len (html-escaped-length s len)]
        (if (neg? out-len)
          s
          (escape-html-sb s len (sb/create-string-builder out-len)))))))

(defn- json-escaped-length
  "Exact length of the JSON-escaped form of `s`, or -1 when no character needs
   escaping. The per-char additions match escape-json-sb: \" \\ / \\b \\f \\n \\r
   \\t each grow by 1, any other control char (< 32) becomes \\uXXXX (+5)."
  [^String s len]
  (loop [i 0 extra 0]
    (if (< i len)
      (let [c (.charAt s i)]
        (recur (inc i)
               (+ extra (long (case c
                                (\" \\ \/ \backspace \formfeed \newline \return \tab) 1
                                (if (< (int c) 32) 5 0))))))
      (if (zero? extra) -1 (+ len extra)))))

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

(defrecord Json []
  sanitizer/Sanitizer
  (sanitize [_ s]
    (when s
      (let [len (count s)
            out-len (json-escaped-length s len)]
        (if (neg? out-len)
          s
          (escape-json-sb s len (sb/create-string-builder out-len)))))))

(defrecord None []
  sanitizer/Sanitizer
  (sanitize [_ s] s))
