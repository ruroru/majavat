(ns jj.majavat
  (:require
    [clojure.tools.logging :as logger]
    [jj.majavat.error-handler.reporting :as reporting]
    [jj.majavat.protocol.builder :as builder]
    [jj.majavat.renderer :refer [->StringRenderer]]
    [jj.majavat.renderer.json :as json]
    [jj.majavat.renderer.yaml :as yaml]
    [jj.majavat.renderer.sanitizer :as sanitizer]
    [jj.majavat.protocol.builder :as builder]
    [jj.majavat.builder :as builders]
    [jj.majavat.resolver.resource :as rcr]
    [jj.majavat.resolver.string :as str-resolver]))

(def ^:private default-resolver (delay (rcr/->ResourceResolver)))

(def ^:private string-template-path "<string>")

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

         environment (-> (get opts :environment {})
                         (update :json-serializer #(or % (json/->DefaultJsonSerializer)))
                         (update :yaml-serializer #(or % (yaml/->DefaultYamlSerializer)))
                         (assoc :fragment (:fragment opts)))

         renderer (or (:renderer opts)
                      (->StringRenderer))

         cache? (get opts :cache? true)
         pre-render-context (if (map? (get opts :pre-render {}))
                              (get opts :pre-render {})
                              (logger/errorf "pre-render is not a map"))
         sanitizer (get opts :sanitizer (sanitizer/->None))
         error-handler (or (:error-handler opts)
                           (reporting/->Reporting))
         build-builder (or (:builder opts)
                           (if cache?
                             builders/->CachedBuilder
                             builders/->OneShotBuilder))
         builder (build-builder pre-render-context environment)]

     (builder/build-renderer builder file-path resolver renderer sanitizer error-handler))))

(defn build-string-renderer
  ([template]
   (build-string-renderer template {}))
  ([template opts]
   (build-renderer string-template-path
                   (assoc opts :template-resolver
                              (str-resolver/->StringResolver string-template-path template)))))

(defn build-html-renderer
  ([file-path]
   (build-renderer file-path {:sanitizer (sanitizer/->Html)}))
  ([file-path opts]
   (build-renderer file-path (assoc opts :sanitizer (sanitizer/->Html)))))
