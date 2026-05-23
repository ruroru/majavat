(ns jj.majavat.protocol.mock-dictionary
  (:require [jj.majavat.protocol.dictionary :refer [Dictionary]]))

(defrecord MockDictionary [translations]
  Dictionary
  (translate [_ language word]
    (get-in translations [language word])))

(defn create-mock-dictionary []
  (->MockDictionary {"en" {:hello "hello" :world "world" :key "key"}
                      "fi" {:hello "hei" :world "maailma" :key "avain"}}))
