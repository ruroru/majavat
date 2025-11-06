(ns jj.majavat.builder
  (:require [jj.majavat.parser :as parser]
            [jj.majavat.renderer :as renderer]))

(defprotocol Builder
  (build-renderer [this file-path template-resolver renderer]))


(defrecord CachedBuilder []
  Builder
  (build-renderer [this file-path template-resolver renderer]
    (let [template (parser/parse file-path template-resolver)]
      (fn [context]
        (renderer/render renderer template context)))))


(defrecord OneShotBuilder []
  Builder
  (build-renderer [this file-path template-resolver renderer]
    (fn [context]
      (let [template (parser/parse file-path template-resolver)]
        (renderer/render renderer template context)))))

