(ns jj.majavat
  (:require
    [jj.majavat.parser :as parser]
    [jj.majavat.renderer :as renderer :refer [->InputStreamRenderer ->StringRenderer]]
    [jj.majavat.resolver.resource :as rcr]))

(def default-resolver (rcr/->ResourceResolver))

(defn render
  ([file-path]
   (render file-path {}))
  ([file-path {:keys [return-type
                      template-resolver
                      sanitizer
                      cache?
                      ]
               :or   {return-type       :string
                      template-resolver default-resolver
                      sanitizer         nil
                      cache?            true}}]
   (if cache?
     (let [template (parser/parse file-path template-resolver)
           rend (if (= :input-stream return-type)
                  (->InputStreamRenderer {:sanitizer sanitizer})
                  (->StringRenderer {:sanitizer sanitizer}))]

       (fn [context]
         (renderer/render rend template context)))
     (fn [context]
       (let [new-template (parser/parse file-path template-resolver)
             renderer (if (= :input-stream return-type)
                        (->InputStreamRenderer {:sanitizer sanitizer})
                        (->StringRenderer {:sanitizer sanitizer}))]
         (renderer/render renderer new-template context))))))