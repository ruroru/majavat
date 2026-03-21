(ns jj.majavat.protocol.renderer.render-target)

(defprotocol RenderTarget
  (render [this template context sanitizer]))