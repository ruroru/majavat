(ns jj.majavat.renderer.escape )

(defprotocol CharEscaper
  (escape [_ string]))