(ns jj.majavat
  (:require
    [clojure.tools.logging :as logger]
    [jj.majavat.protocol.builder :as builder]
    [jj.majavat.renderer :refer [->StringRenderer]]
    [jj.majavat.renderer.sanitizer :as sanitizer]
    [jj.majavat.protocol.builder :as builder]
    [jj.majavat.builder :as builders]
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

         environment (get opts :environment {})

         renderer (or (:renderer opts)
                      (->StringRenderer))

         cache? (get opts :cache? true)
         pre-render-context (if (map? (get opts :pre-render {}))
                              (get opts :pre-render {})
                              (logger/errorf "pre-render is not a map"))
         sanitizer (get opts :sanitizer nil)
         builder (if cache?
                   (builders/->CachedBuilder pre-render-context environment)
                   (builders/->OneShotBuilder pre-render-context environment))]

     (builder/build-renderer builder file-path resolver renderer sanitizer))))

(defn build-html-renderer
  ([file-path]
   (build-renderer file-path {:sanitizer (sanitizer/->Html)}))
  ([file-path opts]
   (build-renderer file-path (assoc opts :sanitizer (sanitizer/->Html)))))
