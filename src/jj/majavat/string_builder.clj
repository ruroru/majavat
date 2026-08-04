(ns jj.majavat.string-builder
  "A small mutable string-builder abstraction wrapping java.lang.StringBuilder.
   Kept behind this namespace so the underlying implementation can be swapped
   out later without touching call sites.")

(defn create-string-builder
  "Creates a new, empty string builder. An initial capacity may be given as a hint."
  ([]
   (StringBuilder.))
  ([^long capacity]
   (StringBuilder. capacity)))

(defn append
  "Appends a string or char to the builder and returns the builder.
   Dispatches on the argument type so the call compiles to the matching
   StringBuilder overload instead of a reflective, char-boxing `.append`."
  [^StringBuilder sb s]
  (if (string? s)
    (.append sb ^String s)
    (.append sb (char s))))

(defn build
  "Returns the accumulated string."
  [^StringBuilder sb]
  (.toString sb))
