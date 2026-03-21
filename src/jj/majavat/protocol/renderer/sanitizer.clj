(ns jj.majavat.protocol.renderer.sanitizer)

(defprotocol Sanitizer
  (sanitize [_ input]))
