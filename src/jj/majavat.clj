(ns jj.majavat
  (:require
    [clojure.tools.logging :as logger]
    [jj.majavat.builder :as builder]
    [jj.majavat.renderer :refer [->StringRenderer]]
    [jj.majavat.renderer.sanitizer :refer [->Html]]
    [jj.majavat.resolver.resource :as rcr]))

(def ^:private default-resolver (delay (rcr/->ResourceResolver)))

(defn build-renderer
  ([file-path]
   (build-renderer file-path {}))
  ([file-path opts]
   (let [file-path (or file-path
                       (do
                         (logger/error "File is set to nil")
                         "nil"))

         resolver (or (:template-resolver opts)
                      @default-resolver)

         filters (get opts :filters {})

         renderer (or (:renderer opts)
                      (->StringRenderer))

         cache? (get opts :cache? true)
         pre-render-context (get opts :pre-render {})
         sanitizer (get opts :sanitizer nil)
         builder (if cache?
                   (builder/->CachedBuilder pre-render-context filters)
                   (builder/->OneShotBuilder pre-render-context filters))]

     (builder/build-renderer builder file-path resolver renderer sanitizer))))

(defn build-html-renderer
  ([file-path]
   (build-renderer file-path {:sanitizer (->Html)}))
  ([file-path opts]
   (build-renderer file-path (assoc opts :sanitizer (->Html)))))
