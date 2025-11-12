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
   (let [resolved-file-path (or file-path "nil")
         resolved-renderer (or renderer @default-renderer)
         resolved-resolver (or template-resolver @default-resolver)
         selected-builder (if cache? @cached-builder @one-shot-builder)]
     (builder/build-renderer selected-builder resolved-file-path resolved-resolver resolved-renderer))))
