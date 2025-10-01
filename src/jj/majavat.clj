(ns jj.majavat
  (:require
    [jj.majavat.parser :as parser]
    [jj.majavat.renderer :as renderer :refer [->InputStreamRenderer ->StringRenderer]]
    [jj.majavat.resolver.resource :as rcr]))

(defonce ^:private renderer-cache (atom {}))

(def default-resolver (rcr/->ResourceResolver))

(defn- cache-key [file-path return-type sanitizer]
  [file-path return-type sanitizer])

(defn render
  ([file-path context]
   (render file-path context {}))
  ([file-path context {:keys [return-type
                              template-resolver
                              sanitizer
                              cache?]
                       :or   {return-type       :string
                              template-resolver default-resolver
                              sanitizer         nil
                              cache?            true}}]
   (let [k (cache-key file-path return-type sanitizer)
         cached-renderer (when cache? (get @renderer-cache k))
         rend (or cached-renderer
                  (let [template (parser/parse file-path template-resolver)
                        new-renderer (if (= :input-stream return-type)
                                       (->InputStreamRenderer template {:sanitizer sanitizer})
                                       (->StringRenderer template {:sanitizer sanitizer}))]
                    (when cache?
                      (swap! renderer-cache assoc k new-renderer))
                    new-renderer))]
     (renderer/render rend context))))