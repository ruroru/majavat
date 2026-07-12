(ns jj.majavat.error-handler.fail-fast
  (:require [jj.majavat.protocol.error-handler :as error-handler]))

(defrecord FailFast []
  error-handler/ErrorHandler
  (handle-error [this renderer template]
    (throw (ex-info (format "Failed to render template: %s"
                            (:error-message template))
                    template))))