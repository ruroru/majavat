(ns jj.majavat
  (:require
    [jj.majavat.renderer :as renderer]
    [jj.majavat.parser :as parser]
    [jj.majavat.resource-content-resolver :as rcr]))

(defonce ^:private template-cache (atom {}))

(defn render-file
  ([file-path config]
   (render-file file-path config {}))
  ([file-path config {:keys [return-type
                             content-resolver]
                      :or   {return-type      :string
                             content-resolver (rcr/->ResourceContentResolver)}}]
   (let [template (or (get @template-cache file-path)
                      (let [parsed-template (parser/parse file-path content-resolver)]
                        (swap! template-cache assoc file-path parsed-template)
                        parsed-template))]
     (if (= :input-stream return-type)
       (renderer/render-is template config)
       (renderer/render template config)))))
