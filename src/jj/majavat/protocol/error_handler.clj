(ns jj.majavat.protocol.error-handler)

(defprotocol ErrorHandler
  (handle-error [this renderer template]))