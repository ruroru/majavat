(ns jj.majavat.resolver.string
  (:require [jj.majavat.protocol.resolver :as template-resolver])
  (:import (java.io StringReader)))

(defrecord StringResolver [path template]
  template-resolver/TemplateResolver

  (open-reader [_ content-path]
    (when (= content-path path)
      (StringReader. template)))

  (template-exists? [_ content-path]
    (= content-path path)))
