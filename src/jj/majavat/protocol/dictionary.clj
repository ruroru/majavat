(ns jj.majavat.protocol.dictionary)

(defprotocol Dictionary
  "Protocol for translating keys to values."

  (translate [this language word]
    "Translates the given word in the specified language."))
