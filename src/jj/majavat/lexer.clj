(ns jj.majavat.lexer
  (:require
    [clojure.string :as str]
    [clojure.string :as string]))

(defn- rrest [s]
  (rest (rest s)))

(defn- parse-let-assignment [assignment-string]
  (let [trimmed (string/trim assignment-string)
        parts (string/split trimmed #"\s*=\s*" 2)]
    (if (= 2 (count parts))
      (let [var-name (keyword (string/trim (first parts)))
            var-value-str (string/trim (second parts))
            var-value (if (and (string/starts-with? var-value-str "\"")
                               (string/ends-with? var-value-str "\""))
                        (subs var-value-str 1 (dec (count var-value-str)))
                        var-value-str)]

        {:variable-name var-name :variable-value var-value})
      {:variable-name nil :variable-value nil})))

(defn- tokenize-recursively [my-sequence current-string vector line-number]
  (if-not (empty? my-sequence)
    (let [current-char (first my-sequence)
          next-char (first (next my-sequence))

          new-line-number (if (= current-char \newline)
                            (inc line-number)
                            (if (and (= current-char \return)
                                     (not= next-char \newline))
                              (inc line-number)
                              line-number))]

      (cond
        (and (= current-char \{) (= next-char \{))
        (cond
          (string/blank? current-string)
          (recur (rrest my-sequence) "" (conj vector {:type :opening-bracket}) new-line-number)
          :else
          (recur (rrest my-sequence) "" (conj vector {:type :text :value current-string} {:type :opening-bracket}) new-line-number))

        (and (= current-char \{) (= next-char \%))
        (cond
          (string/blank? current-string)
          (recur (rrest my-sequence) "" (conj vector {:type :block-start}) new-line-number)
          :else
          (recur (rrest my-sequence) "" (conj vector {:type :text :value current-string} {:type :block-start}) new-line-number))

        (and (= current-char \%) (= next-char \}))
        (let [trimmed-string (string/trim current-string)]
          (cond
            (= "endfor" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :end-for} {:type :block-end :line line-number}) new-line-number)

            (= "endlet" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :keyword-end-let} {:type :block-end :line line-number}) new-line-number)

            (= "endif" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :keyword-endif} {:type :block-end :line line-number}) new-line-number)

            (= "else" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :keyword-else} {:type :block-end :line line-number}) new-line-number)

            (and (= "" trimmed-string) (= (:type (last vector)) :keyword-extends))
            (recur (rrest my-sequence) "" (conj vector {:type :block-end :line line-number}) new-line-number)

            (and (= "" trimmed-string) (= (:type (last vector)) :keyword-block))
            (recur (rrest my-sequence) "" (conj vector {:type :block-end :line line-number}) new-line-number)

            (= (:type (last vector)) :keyword-let)
            (recur (rrest my-sequence) "" (conj vector {:type :block-end :line line-number}) new-line-number)

            :else
            (recur (rrest my-sequence) "" (conj vector {:type :block-end :line line-number}) new-line-number)))

        (every? identity [(= "in" (string/trim current-string)) (= (:type (last vector)) :identifier)])
        (recur (rest my-sequence) (str "" current-char) (conj vector {:type :keyword-in}) new-line-number)

        (every? identity [(= (:type (last vector)) :keyword-in) (= next-char \%)])
        (let [current-trimmed (string/trim current-string)]
          (if (not (.isBlank ^String current-trimmed))
            (recur (rest my-sequence) "" (conj vector {:type :identifier :value (mapv keyword (-> current-trimmed
                                                                                                  (str/split #"\.")))}) new-line-number)
            (recur (rest my-sequence) "" (conj vector {:type :identifier}) new-line-number)))

        (every? identity [(= (:type (last vector)) :keyword-in)])
        (recur (rest my-sequence) (str current-string current-char) vector new-line-number)

        (and (= current-char \}) (= next-char \}))
        (let [trimmed-string (string/trim current-string)]
          (cond
            (= (:type (last vector)) :filter-tag)
            (if (not (string/blank? trimmed-string))
              (recur (rrest my-sequence) "" (conj vector {:type :filter-function :value (keyword trimmed-string)}
                                                  {:type :closing-bracket :line line-number}) new-line-number)
              (recur (rrest my-sequence) "" (conj vector {:type :closing-bracket :line line-number}) new-line-number))

            (string/blank? trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :expression}
                                                {:type :closing-bracket :line line-number}) new-line-number)

            :else
            (recur (rrest my-sequence) "" (conj vector {:type  :expression
                                                        :value (mapv keyword (-> trimmed-string
                                                                                 (str/split #"\.")))}
                                                {:type :closing-bracket :line line-number}) new-line-number)))

        (= (:type (last vector)) :opening-bracket)
        (cond
          (and (= current-char \|) (not (string/blank? current-string)))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj vector {:type :expression :value (mapv keyword (-> trimmed-string (str/split #"\.")))} {:type :filter-tag}) new-line-number))
          :else
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :filter-tag)
        (recur (rest my-sequence) (str current-string current-char) vector new-line-number)

        (= (:type (last vector)) :block-start)
        (cond
          (= (string/trim current-string) "for")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-for}) new-line-number)

          (= (string/trim current-string) "let")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-let}) new-line-number)

          (= (string/trim current-string) "if")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-if}) new-line-number)

          (= (string/trim current-string) "include")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-include}) new-line-number)

          (= (string/trim current-string) "endif")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-endif}) new-line-number)

          (= (string/trim current-string) "extends")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-extends}) new-line-number)

          (= (string/trim current-string) "block")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-block}) new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (and (= (:type (last vector)) :keyword-if)
             (= (string/trim current-string) "not")
             (= current-char \ ))
        (let [current-vector (pop vector)]
          (recur (rest my-sequence) "" (conj current-vector {:type :keyword-if-not}) new-line-number))

        (or (= (:type (last vector)) :keyword-if)
            (= (:type (last vector)) :keyword-if-not))
        (if
          (and (= current-char \ ) (not (string/blank? current-string)))
          (recur (rest my-sequence) (str "" current-char) (conj vector {:type :identifier :value (mapv keyword (-> (string/trim current-string)
                                                                                                                   (str/split #"\.")))}) new-line-number)
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :keyword-let)
        (if (= next-char \%)
          (let [{:keys [variable-name variable-value]} (parse-let-assignment (str/trim current-string))]

            (recur (rest my-sequence) "" (conj vector {:type            :variable-declaration
                                                       :variable-name  variable-name
                                                       :variable-value variable-value}) new-line-number))
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :keyword-for)
        (cond
          (and (not (string/blank? current-string))
               (or (= current-char \ ) (= next-char \%)))
          (recur (rest my-sequence) (str "" current-char) (conj vector {:type :identifier :value (keyword (string/trim current-string))}) new-line-number)
          :else
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :keyword-include)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string vector new-line-number)

          (and (string/blank? current-string) (= current-char \"))
          (recur (rest my-sequence) "" vector new-line-number)

          (and (not (string/blank? current-string)) (= current-char \"))
          (recur (rest my-sequence) "" (conj vector {:type :file-path :value current-string}) new-line-number)

          (not (= current-char \"))
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :keyword-extends)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string vector new-line-number)

          (and (not (string/blank? current-string)) (not= current-char \ ))
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number)

          (and (not (string/blank? current-string)) (= current-char \ ))
          (recur (rest my-sequence) "" (conj vector {:type :extends-block-name :value (keyword (string/trim current-string))}) new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :extends-block-name)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string vector new-line-number)

          (and (string/blank? current-string) (= current-char \"))
          (recur (rest my-sequence) "" vector new-line-number)

          (and (not (string/blank? current-string)) (= current-char \"))
          (recur (rest my-sequence) "" (conj vector {:type :file-path :value current-string}) new-line-number)

          (not (= current-char \"))
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :keyword-block)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string vector new-line-number)

          (and (not (string/blank? current-string))
               (or (= current-char \ ) (= next-char \%)))
          (recur (rest my-sequence) (str "" current-char) (conj vector {:type :block-name :value (keyword (string/trim current-string))}) new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        :else
        (recur (rest my-sequence) (str current-string current-char) vector new-line-number)))

    (if-not (string/blank? current-string)
      (conj vector {:type :text :value current-string})
      vector)))

(defn tokenize [string]
  (tokenize-recursively (seq string) "" [] 1))