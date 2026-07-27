(ns jj.majavat.resolver.string
  (:require [jj.majavat.protocol.resolver :as template-resolver]))

(defrecord StringResolver [path template]
  template-resolver/TemplateResolver

  (read-template [_ content-path]
    (when (= content-path path)
      template))

  (template-exists? [_ content-path]
    (= content-path path)))
