(ns jj.majavat
  (:require
    [jj.majavat.parser :as parser]
    [jj.majavat.renderer :as renderer :refer [->StringRenderer]]
    [jj.majavat.resolver.resource :as rcr]))

(def ^:private default-resolver (rcr/->ResourceResolver))
(def ^:private default-renderer (->StringRenderer {}))

(defn build-renderer
  ([file-path]
   (build-renderer file-path {}))
  ([file-path {:keys [template-resolver
                      cache?
                      renderer]
               :or   {template-resolver default-resolver
                      cache?            true
                      renderer          default-renderer}}]

   (if cache?
     (let [template (parser/parse file-path template-resolver)]
       (fn [context]
         (renderer/render renderer template context)))

     (fn [context]
       (let [template (parser/parse file-path template-resolver)]
         (renderer/render renderer template context))))))