(ns jj.majavat.renderer.escape.html
  (:require [jj.majavat.renderer.sanitizer])
  (:import (jj.majavat.renderer.sanitizer Sanitizer)))

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