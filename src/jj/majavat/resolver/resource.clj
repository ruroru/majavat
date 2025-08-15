(ns jj.majavat.resolver.resource
  (:require [clojure.java.io :as io]
            [jj.majavat.resolver :as cr])
  (:import (java.nio.file Paths)))

(defrecord ResourceContentResolver []
  cr/ContentResolver
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
    (when-let [resource (io/resource content-path)]
      (slurp resource)))

  (content-exists? [_ content-path]
    (some? (io/resource content-path))))