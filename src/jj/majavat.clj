(ns jj.majavat
  (:require
    [jj.majavat.builder :as builder]
    [jj.majavat.renderer :refer [->StringRenderer]]
    [jj.majavat.resolver.resource :as rcr]))

(def ^:private default-resolver (delay (rcr/->ResourceResolver)))
(def ^:private default-renderer (delay (->StringRenderer {})))
(def ^:private cached-builder (delay (builder/->CachedBuilder)))
(def ^:private one-shot-builder (delay (builder/->OneShotBuilder)))

(defn build-renderer
  ([file-path]
   (build-renderer file-path {}))
  ([file-path {:keys [template-resolver
                      cache?
                      renderer]
               :or   {template-resolver @default-resolver
                      cache?            true
                      renderer          @default-renderer}}]

   (if cache?
     (builder/build-renderer @cached-builder file-path template-resolver renderer)
     (builder/build-renderer @one-shot-builder file-path template-resolver renderer))))