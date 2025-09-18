(ns jj.majavat.renderer.sanitizer )

(defprotocol Sanitizer
  (sanitize [_ input]))