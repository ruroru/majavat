(ns jj.majavat
  (:require
    [jj.majavat.renderer :as renderer]
    [jj.majavat.parser :as parser]))

(defonce template-cache (atom {}))

(defn render-file
  ([file-path config]
   (let [template (or (get @template-cache file-path)
                      (let [parsed-template (parser/parse file-path)]
                        (swap! template-cache assoc file-path parsed-template)
                        parsed-template))]
     (renderer/render template config))))
