(ns jj.majavat.cache
  (:require [jj.majavat :as majavat]))

(defonce ^:private cache (atom {}))

(defn render
  ([filename data]
   (render filename data {}))

  ([filename data opts]
   (let [renderer (or (get @cache filename)
                      (let [f (majavat/build-renderer filename opts)]
                        (swap! cache assoc filename f)
                        f))]
     (renderer data))))

(defn render-html
  ([filename data]
   (render-html filename data {}))

  ([filename data opts]
   (let [renderer (or (get @cache filename)
                      (let [f (majavat/build-html-renderer filename opts)]
                        (swap! cache assoc filename f)
                        f))]
     (renderer data))))

(defn clear-cache!
  "Clear all cached renderers."
  []
  (reset! cache {}))

(defn clear-file!
  "Remove a specific file from cache."
  [filename]
  (swap! cache dissoc filename))