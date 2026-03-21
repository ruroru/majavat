(ns jj.majavat.resolver.fs
  (:require [jj.majavat.protocol.resolver :as template-resolver])
  (:import
    (java.nio.charset StandardCharsets)
    (java.nio.file Files LinkOption Paths)))

(defrecord FsResolver []
  template-resolver/TemplateResolver

  (open-reader [_ content-path]
    (let [path (Paths/get content-path (make-array String 0))]
      (when (Files/exists path (make-array LinkOption 0))
        (Files/newBufferedReader path StandardCharsets/UTF_8))))

  (template-exists? [_ content-path]
    (let [path (Paths/get content-path (make-array String 0))]
      (Files/exists path (make-array LinkOption 0)))))
