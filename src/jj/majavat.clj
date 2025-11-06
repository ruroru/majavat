(ns jj.majavat
  (:require
    [jj.majavat.builder :as builder]
    [jj.majavat.renderer :refer [->StringRenderer]]
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
     (builder/build-renderer (builder/->CachedBuilder) file-path template-resolver renderer)
     (builder/build-renderer (builder/->OneShotBuilder) file-path template-resolver renderer))))