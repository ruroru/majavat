(ns jj.majavat.error-handler.reporting
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [jj.majavat.error-handler.fail-fast :as fail-fast]
            [jj.majavat.protocol.error-handler :as error-handler]
            [jj.majavat.protocol.renderer.render-target :as render-target])
  (:import (java.io PushbackReader)))

(defn- read-edn-resource [resource-path]
  (when-let [resource (io/resource resource-path)]
    (with-open [stream (.openStream resource)
                reader (io/reader stream)
                pushback-reader (PushbackReader. reader)]
      (edn/read pushback-reader))))

(defrecord Reporting []
  error-handler/ErrorHandler
  (handle-error [this renderer template sanitizer]
    (render-target/render renderer (read-edn-resource "jj/majavat/error-template.edn") template sanitizer (fail-fast/->FailFast))))