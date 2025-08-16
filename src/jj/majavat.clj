(ns jj.majavat
  (:require
    [jj.majavat.renderer :as renderer]
    [jj.majavat.renderer.ops.html :as ho]
    [jj.majavat.parser :as parser]
    [jj.majavat.resolver.resource :as rcr]))

(defonce ^:private template-cache (atom {}))

(defn render-file
  ([file-path config]
   (render-file file-path config {}))
  ([file-path config {:keys [return-type
                             template-resolver
                             ops
                             cache?
                             escape?]
                      :or   {return-type       :string
                             template-resolver (rcr/->ResourceResolver)
                             ops               (ho/->HtmlOps)
                             cache?            true
                             escape?           true}}]

   (let [template (if cache?
                    (or (get @template-cache file-path)
                        (let [parsed-template (parser/parse file-path template-resolver)]
                          (swap! template-cache assoc file-path parsed-template)
                          parsed-template))
                    (parser/parse file-path template-resolver))]

     (if (= :input-stream return-type)
       (renderer/render-is template config {:escape? escape?
                                            :ops     ops})
       (renderer/render template config {:escape? escape?
                                         :ops     ops})))))
