(ns jj.majavat
  (:require
    [clojure.tools.logging :as logger]
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
  ([file-path opts]
   (let [template-resolver (get opts :template-resolver @default-resolver)
         cache? (get opts :cache? true)
         renderer (get opts :renderer @default-renderer)
         resolved-file-path (or file-path
                                (do
                                  (logger/error "File is set to nil")
                                  "nil"))
         resolved-renderer (or renderer
                               (do
                                 (logger/error "Renderer is set to nil, defaulting to string renderer")
                                 @default-renderer))
         resolved-resolver (or template-resolver
                               (do
                                 (logger/error "Resolver is set to nil, defaulting to resource resolver")
                                 @default-resolver))
         selected-builder (if cache? @cached-builder @one-shot-builder)]
     (builder/build-renderer selected-builder resolved-file-path resolved-resolver resolved-renderer))))