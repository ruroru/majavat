(ns jj.majavat.string-builder)

(defn create-string-builder
  ([]
   (StringBuilder.))
  ([^long capacity]
   (StringBuilder. capacity)))

(defn build
  [^StringBuilder sb]
  (.toString sb))

(defmacro append
  "Appends a string and returns the builder. Use `append-char` for chars."
  [sb s]
  (let [b (with-meta (gensym "sb") {:tag `StringBuilder})
        v (with-meta (gensym "s") {:tag `String})]
    `(let [~b ~sb
           ~v ~s]
       (.append ~b ~v))))

(defmacro append-char
  "Appends a char and returns the builder. Use `append` for strings."
  [sb c]
  (let [b (with-meta (gensym "sb") {:tag `StringBuilder})]
    `(let [~b ~sb]
       (.append ~b (char ~c)))))
