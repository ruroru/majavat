(ns jj.majavat.resolver.resource
  (:require [clojure.java.io :as io]
            [jj.majavat.protocol.resolver :as template-resolver]))

(defrecord ResourceResolver []
  template-resolver/TemplateResolver

  (read-template [_ content-path]
    (when-let [resource (io/resource content-path)]
      (slurp resource)))

  (template-exists? [_ content-path]
    (some? (io/resource content-path))))
