(ns jj.majavat.resolver.fs
  (:require [jj.majavat.resolver :as cr]
            [jj.majavat.resolver])
  (:import (java.nio.file Files LinkOption Paths)))


(defrecord FsResolver []
  cr/TemplateResolver

  (resolve-path [_ base-path relative-path]
    (let [base-path-obj (Paths/get base-path (make-array String 0))
          relative-path-obj (Paths/get relative-path (make-array String 0))
          parent-path (or (.getParent base-path-obj)
                          (Paths/get "" (make-array String 0)))]

      (-> parent-path
          (.resolve relative-path-obj)
          (.normalize)
          (.toString))))

  (read-content [_ content-path]
    (let [path (Paths/get content-path (make-array String 0))]
      (when (Files/exists path (make-array LinkOption 0))
        (slurp (.toFile path) :encoding "UTF-8"))))


  (content-exists? [_ content-path]
    (let [path (Paths/get content-path (make-array String 0))]
      (Files/exists path (make-array LinkOption 0)))))
