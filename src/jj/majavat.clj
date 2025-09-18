(ns jj.majavat
  (:require
    [jj.majavat.renderer :as renderer]
    [jj.majavat.parser :as parser]
    [jj.majavat.resolver.resource :as rcr]))

(defonce ^:private template-cache (atom {}))

(def default-resolver (rcr/->ResourceResolver))

(defn render-file
  ([file-path config]
   (render-file file-path config {}))
  ([file-path config {:keys [return-type
                             template-resolver
                             sanitizer
                             cache?]
                      :or   {return-type       :string
                             template-resolver default-resolver
                             sanitizer nil
                             cache?            true}}]

   (let [template (if cache?
                    (or (get @template-cache file-path)
                        (let [parsed-template (parser/parse file-path template-resolver)]
                          (swap! template-cache assoc file-path parsed-template)
                          parsed-template))
                    (parser/parse file-path template-resolver))]

     (if (= :input-stream return-type)
       (renderer/render-is template config {:sanitizer sanitizer})
       (renderer/render template config {:sanitizer sanitizer})))))
