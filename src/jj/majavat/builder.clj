(ns jj.majavat.builder
  (:require [jj.majavat.parser :as parser]
            [jj.majavat.renderer :as renderer]))

(defprotocol Builder
  (build-renderer [this file-path template-resolver renderer ]))


(defrecord CachedBuilder [pre-render-context filters]
  Builder
  (build-renderer [this file-path template-resolver renderer ]
    (let [pre-render? (not (empty? pre-render-context))]
      (if pre-render?
        (let [template (parser/parse file-path template-resolver filters)
              ast-renderer (renderer/->PartialRenderer {})]
          (fn [context]
            (renderer/render renderer (renderer/render ast-renderer template pre-render-context) context)))
        (let [template (parser/parse file-path template-resolver filters)]
          (fn [context]
            (renderer/render renderer template context)))))))


(defrecord OneShotBuilder [filters]
  Builder
  (build-renderer [this file-path template-resolver renderer ]
    (fn [context]
      (let [template (parser/parse file-path template-resolver filters)]
        (renderer/render renderer template context)))))

