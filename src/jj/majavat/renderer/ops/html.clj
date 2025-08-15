(ns jj.majavat.renderer.ops.html
  (:require [jj.majavat.renderer.ops ])
  (:import (jj.majavat.renderer.ops Ops)))

(defrecord HtmlOps []
  Ops
  (escape [_  s]
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