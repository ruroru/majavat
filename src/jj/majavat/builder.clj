(ns jj.majavat.builder
  (:require [jj.majavat.parser :as parser]
            [jj.majavat.protocol.builder :as builder]
            [jj.majavat.renderer :as renderers]
            [jj.majavat.protocol.renderer.render-target :as renderer]))


(defrecord CachedBuilder [pre-render-context filters sanitizers]
  builder/Builder
  (build-renderer [this file-path template-resolver renderer sanitizer]
    (let [pre-render? (not (empty? pre-render-context))]
      (if pre-render?
        (let [template (parser/parse file-path template-resolver filters sanitizers)
              ast-renderer (renderers/->PartialRenderer)]
          (fn [context]
            (renderer/render renderer (renderer/render ast-renderer template pre-render-context sanitizer) context sanitizer)))
        (let [template (parser/parse file-path template-resolver filters sanitizers)]
          (fn [context]
            (renderer/render renderer template context sanitizer)))))))


(defrecord OneShotBuilder [pre-render-context filters sanitizers]
  builder/Builder
  (build-renderer [this file-path template-resolver renderer sanitizer]
    (let [pre-render? (not (empty? pre-render-context))]
      (if pre-render?
        (fn [context]
          (let [template (parser/parse file-path template-resolver filters sanitizers)
                merged-context (merge context pre-render-context)]
            (renderer/render renderer template merged-context sanitizer)))
        (fn [context]
          (let [template (parser/parse file-path template-resolver filters sanitizers)]
            (renderer/render renderer template context sanitizer)))))))

