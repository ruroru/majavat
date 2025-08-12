(ns jj.majavat.lexer
  (:require
    [clojure.string :as str]
    [clojure.string :as string]))

(defn- rrest [s]
  (rest (rest s)))

(def empty-string "")

(defn- tokenize-recursively [my-sequence current-string vector]
  (if-not (empty? my-sequence)
    (let [current-char (first my-sequence)
          next-char (first (next my-sequence))]

      (cond
        (and (= current-char \{) (= next-char \{))
        (cond
          (string/blank? current-string)
          (recur (rrest my-sequence) empty-string (conj vector {:type :opening-bracket}))
          :else
          (recur (rrest my-sequence) empty-string (conj vector {:type :text :value current-string} {:type :opening-bracket})))

        (and (= current-char \{) (= next-char \%))
        (cond
          (string/blank? current-string)
          (recur (rrest my-sequence) empty-string (conj vector {:type :block-start}))
          :else
          (recur (rrest my-sequence) empty-string (conj vector {:type :text :value current-string} {:type :block-start})))

        (and (= current-char \%) (= next-char \}))
        (let [trimmed-string (string/trim current-string)]
          (cond
            (= "endfor" trimmed-string)
            (recur (rrest my-sequence) empty-string (conj vector {:type :end-for} {:type :block-end}))

            (= "endif" trimmed-string)
            (recur (rrest my-sequence) empty-string (conj vector {:type :keyword-endif} {:type :block-end}))

            (= "else" trimmed-string)
            (recur (rrest my-sequence) empty-string (conj vector {:type :keyword-else} {:type :block-end}))

            (and (= "" trimmed-string) (= (:type (last vector)) :keyword-extends))
            (recur (rrest my-sequence) empty-string (conj vector {:type :block-end}))

            (and (= "" trimmed-string) (= (:type (last vector)) :keyword-block))
            (recur (rrest my-sequence) empty-string (conj vector {:type :block-end}))

            :else
            (recur (rrest my-sequence) empty-string (conj vector {:type :block-end}))))

        (every? identity [(= "in" (string/trim current-string)) (= (:type (last vector)) :identifier)])
        (recur (rest my-sequence) (str "" current-char) (conj vector {:type :keyword-in}))

        (every? identity [(= (:type (last vector)) :keyword-in) (= next-char \%)])
        (recur (rest my-sequence) empty-string (conj vector {:type :identifier :value (mapv keyword (-> (string/trim current-string)
                                                                                                        (str/split #"\.")))}))

        (every? identity [(= (:type (last vector)) :keyword-in)])
        (recur (rest my-sequence) (str current-string current-char) vector)

        (and (= current-char \}) (= next-char \}))
        (recur (rrest my-sequence) empty-string (conj vector {:type  :expression
                                                              :value (mapv keyword (-> (string/trim current-string)
                                                                                       (str/split #"\.")))}
                                                      {:type :closing-bracket}))

        (= (:type (last vector)) :opening-bracket)
        (recur (rest my-sequence) (str current-string current-char) vector)

        (= (:type (last vector)) :block-start)
        (cond
          (= (string/trim current-string) "for")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-for}))

          (= (string/trim current-string) "if")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-if}))

          (= (string/trim current-string) "include")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-include}))

          (= (string/trim current-string) "endif")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-endif}))

          (= (string/trim current-string) "extends")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-extends}))

          (= (string/trim current-string) "block")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-block}))

          :else
          (recur (rest my-sequence) (str current-string current-char) vector))

        (= (:type (last vector)) :keyword-if)
        (if
          (and (= current-char \ ) (not (string/blank? current-string)))
          (recur (rest my-sequence) (str "" current-char) (conj vector {:type :identifier :value (mapv keyword (-> (string/trim current-string)
                                                                                                                   (str/split #"\.")))}))
          (recur (rest my-sequence) (str current-string current-char) vector))

        (= (:type (last vector)) :keyword-for)
        (cond
          (and (not (string/blank? current-string))
               (or (= current-char \ ) (= next-char \%)))
          (recur (rest my-sequence) (str "" current-char) (conj vector {:type :identifier :value (keyword (string/trim current-string))}))
          :else
          (recur (rest my-sequence) (str current-string current-char) vector))

        (= (:type (last vector)) :keyword-include)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string vector)

          (and (string/blank? current-string) (= current-char \"))
          (recur (rest my-sequence) "" vector)

          (and (not (string/blank? current-string)) (= current-char \"))
          (recur (rest my-sequence) "" (conj vector {:type :file-path :value current-string}))

          (not (= current-char \"))
          (recur (rest my-sequence) (str current-string current-char) vector)

          :else
          (recur (rest my-sequence) (str current-string current-char) vector))

        (= (:type (last vector)) :keyword-extends)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string vector)

          (and (not (string/blank? current-string)) (not= current-char \ ))
          (recur (rest my-sequence) (str current-string current-char) vector)

          (and (not (string/blank? current-string)) (= current-char \ ))
          (recur (rest my-sequence) "" (conj vector {:type :extends-block-name :value (keyword (string/trim current-string))}))

          :else
          (recur (rest my-sequence) (str current-string current-char) vector))

        (= (:type (last vector)) :extends-block-name)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string vector)

          (and (string/blank? current-string) (= current-char \"))
          (recur (rest my-sequence) "" vector)

          (and (not (string/blank? current-string)) (= current-char \"))
          (recur (rest my-sequence) "" (conj vector {:type :file-path :value current-string}))

          (not (= current-char \"))
          (recur (rest my-sequence) (str current-string current-char) vector)

          :else
          (recur (rest my-sequence) (str current-string current-char) vector))

        (= (:type (last vector)) :keyword-block)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string vector)

          (and (not (string/blank? current-string))
               (or (= current-char \ ) (= next-char \%)))
          (recur (rest my-sequence) (str "" current-char) (conj vector {:type :block-name :value (keyword (string/trim current-string))}))

          :else
          (recur (rest my-sequence) (str current-string current-char) vector))

        :else
        (recur (rest my-sequence) (str current-string current-char) vector)))

    (if-not (string/blank? current-string)
      (conj vector {:type :text :value current-string})
      vector)))

(defn tokenize [string]
  (tokenize-recursively (seq string) empty-string []))