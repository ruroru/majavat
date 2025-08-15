(ns jj.majavat.renderer.ops)

(defprotocol Ops
  (escape [_ string]))