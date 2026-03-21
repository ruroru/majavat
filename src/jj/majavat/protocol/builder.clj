(ns jj.majavat.protocol.builder)

(defprotocol Builder
  (build-renderer [this file-path template-resolver renderer escape-config]))
