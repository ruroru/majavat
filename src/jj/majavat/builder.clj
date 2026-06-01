(ns jj.majavat.builder
  (:require [jj.majavat.parser :as parser]
            [jj.majavat.protocol.builder :as builder]
            [jj.majavat.renderer :as renderers]
            [jj.majavat.protocol.renderer.render-target :as renderer]))


(defrecord CachedBuilder [pre-render-context environment]
  builder/Builder
  (build-renderer [this file-path template-resolver renderer sanitizer error-handler]
    (let [{:keys [filters sanitizers dictionary]} environment
          pre-render? (not (empty? pre-render-context))]
      (if pre-render?
        (let [template (parser/parse file-path template-resolver filters sanitizers dictionary)
              ast-renderer (renderers/->PartialRenderer)]
          (fn [context]
            (renderer/render renderer (renderer/render ast-renderer template pre-render-context sanitizer error-handler) context sanitizer error-handler)))
        (let [template (parser/parse file-path template-resolver filters sanitizers dictionary)]
          (fn [context]
            (renderer/render renderer template context sanitizer error-handler)))))))


(defrecord OneShotBuilder [pre-render-context environment]
  builder/Builder
  (build-renderer [this file-path template-resolver renderer sanitizer error-handler]
    (let [{:keys [filters sanitizers dictionary]} environment
          pre-render? (not (empty? pre-render-context))]
      (if pre-render?
        (fn [context]
          (let [template (parser/parse file-path template-resolver filters sanitizers dictionary)
                merged-context (merge context pre-render-context)]
            (renderer/render renderer template merged-context sanitizer error-handler)))
        (fn [context]
          (let [template (parser/parse file-path template-resolver filters sanitizers dictionary)]
            (renderer/render renderer template context sanitizer error-handler)))))))
