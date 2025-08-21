(ns jj.majavat.resolver.resource
  (:require [clojure.java.io :as io]
            [jj.majavat.resolver :as cr]))

(defrecord ResourceResolver []
  cr/TemplateResolver

  (open-reader [_ content-path]
    (when-let [resource (io/resource content-path)]
      (let [stream (.openStream resource)]
        (io/reader stream))))

  (template-exists? [_ content-path]
    (some? (io/resource content-path))))