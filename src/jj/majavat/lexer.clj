(ns jj.majavat.lexer
  (:require [clojure.string :as string]))

(defn- rrest [s]
  (rest (rest s)))

(defn- parse-path [^String s]
  (let [len (.length s)]
    (loop [i 0
           current (StringBuilder.)
           result []]
      (if (>= i len)
        (let [remaining (.toString current)]
          (if (string/blank? remaining)
            result
            (conj result (keyword remaining))))
        (let [c (.charAt s i)]
          (cond
            (= c \[)
            (let [close (.indexOf s "]" (int i))]
              (if (pos? close)
                (let [content (subs s (inc i) close)
                      next-i (if (and (< (inc close) len) (= (.charAt s (inc close)) \.))
                               (+ close 2)
                               (inc close))]
                  (recur next-i (StringBuilder.) (conj result (keyword content))))
                (recur (inc i) (.append current c) result)))

            (= c \.)
            (let [remaining (.toString current)]
              (if (string/blank? remaining)
                (recur (inc i) (StringBuilder.) result)
                (recur (inc i) (StringBuilder.) (conj result (keyword remaining)))))

            :else
            (recur (inc i) (.append current c) result)))))))

(defn- parse-let-assignment [assignment-string]
  (let [trimmed (string/trim assignment-string)
        parts (string/split trimmed #"\s*=\s*" 2)]
    (if (= 2 (count parts))
      (let [var-name (keyword (string/trim (first parts)))
            var-value-str (string/trim (second parts))
            var-value (if (and (string/starts-with? var-value-str "\"")
                               (string/ends-with? var-value-str "\""))
                        (subs var-value-str 1 (dec (count var-value-str)))
                        (parse-path var-value-str))]

        {:variable-name var-name :variable-value var-value})
      {:variable-name nil :variable-value nil})))

(defn- parse-query-string-value [value-string]
  (let [trimmed (string/trim value-string)]
    (if (and (string/starts-with? trimmed "\"")
             (string/ends-with? trimmed "\""))
      (subs trimmed 1 (dec (count trimmed)))
      (parse-path trimmed))))

(defn- skip-comment [my-sequence line-number]
  (loop [seq-rest my-sequence
         current-line line-number]
    (if (empty? seq-rest)
      [seq-rest current-line]
      (let [current-char (first seq-rest)
            next-char (first (next seq-rest))
            new-line-number (if (= current-char \newline)
                              (inc current-line)
                              (if (and (= current-char \return)
                                       (not= next-char \newline))
                                (inc current-line)
                                current-line))]
        (if (and (= current-char \#) (= next-char \}))
          [(rrest seq-rest) new-line-number]
          (recur (rest seq-rest) new-line-number))))))

(defn- collect-verbatim-content [my-sequence line-number]
  (loop [seq-rest my-sequence
         current-line line-number
         accumulated ""]
    (if (empty? seq-rest)
      [seq-rest current-line accumulated]
      (let [current-char (first seq-rest)
            next-char (first (next seq-rest))
            new-line-number (if (= current-char \newline)
                              (inc current-line)
                              (if (and (= current-char \return)
                                       (not= next-char \newline))
                                (inc current-line)
                                current-line))]
        (if (and (= current-char \{) (= next-char \%))
          (let [remaining (drop 2 seq-rest)
                tag-str (apply str (take-while #(not= % \%) remaining))]
            (if (= (string/trim tag-str) "endverbatim")
              [seq-rest new-line-number accumulated]
              (recur (rest seq-rest) new-line-number (str accumulated current-char))))
          (recur (rest seq-rest) new-line-number (str accumulated current-char)))))))

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
        (and (= current-char \{) (= next-char \#))
        (let [[remaining-seq updated-line-number] (skip-comment (rrest my-sequence) new-line-number)]
          (if (empty? current-string)
            (recur remaining-seq "" vector updated-line-number)
            (recur remaining-seq "" (conj vector {:type :text :value current-string}) updated-line-number)))

        (and (= current-char \{) (= next-char \{))
        (cond
          (empty? current-string)
          (recur (rrest my-sequence) "" (conj vector {:type :opening-bracket}) new-line-number)
          :else
          (recur (rrest my-sequence) "" (conj vector {:type :text :value current-string} {:type :opening-bracket}) new-line-number))

        (and (= current-char \{) (= next-char \%))
        (cond
          (empty? current-string)
          (recur (rrest my-sequence) "" (conj vector {:type :block-start}) new-line-number)
          :else
          (recur (rrest my-sequence) "" (conj vector {:type :text :value current-string} {:type :block-start}) new-line-number))

        (and (= current-char \%) (= next-char \}))
        (let [trimmed-string (string/trim current-string)]
          (cond
            (= "endfor" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :end-for} {:type :block-end :line line-number}) new-line-number)

            (= "endeach" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :end-each-token} {:type :block-end :line line-number}) new-line-number)

            (= "endverbatim" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :end-verbatim} {:type :block-end :line line-number}) new-line-number)

            (= "endlet" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :keyword-end-let} {:type :block-end :line line-number}) new-line-number)

            (= "endescape" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :keyword-end-escape} {:type :block-end :line line-number}) new-line-number)

            (= "csrf-token" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :keyword-csrf-token} {:type :block-end :line line-number}) new-line-number)

            (= "now" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :now} {:type :block-end :line line-number}) new-line-number)

            (= "endif" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :keyword-endif} {:type :block-end :line line-number}) new-line-number)

            (= "else" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :keyword-else} {:type :block-end :line line-number}) new-line-number)

            (= "empty" trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :keyword-empty} {:type :block-end :line line-number}) new-line-number)

            (and (= "" trimmed-string) (= (:type (last vector)) :keyword-extends))
            (recur (rrest my-sequence) "" (conj vector {:type :block-end :line line-number}) new-line-number)

            (and (= "" trimmed-string) (= (:type (last vector)) :keyword-block))
            (recur (rrest my-sequence) "" (conj vector {:type :content} {:type :block-end :line line-number}) new-line-number)

            (= (:type (last vector)) :keyword-let)
            (recur (rrest my-sequence) "" (conj vector {:type :block-end :line line-number}) new-line-number)

            (= (:type (last vector)) :keyword-escape)
            (recur (rrest my-sequence) "" (conj vector {:type :escape-name :value (keyword trimmed-string)} {:type :block-end :line line-number}) new-line-number)

            (= (:type (last vector)) :keyword-query-string)
            (recur (rrest my-sequence) "" (conj vector {:type :block-end :line line-number}) new-line-number)

            (= (:type (last vector)) :now)
            (recur (rrest my-sequence) "" (conj vector {:type :block-end :line line-number}) new-line-number)

            (and (map? (last vector)) (:now-format (last vector)))
            (recur (rrest my-sequence) "" (conj vector {:type :block-end :line line-number}) new-line-number)

            (= (:type (last vector)) :verbatim)
            (let [[remaining-seq updated-line-number verbatim-content] (collect-verbatim-content (rrest my-sequence) new-line-number)]
              (if (empty? verbatim-content)
                (recur remaining-seq "" (conj vector {:type :block-end :line line-number}) updated-line-number)
                (recur remaining-seq "" (conj vector {:type :block-end :line line-number} {:type :text :value verbatim-content}) updated-line-number)))

            :else
            (recur (rrest my-sequence) "" (conj vector {:type :block-end :line line-number}) new-line-number)))

        (every? identity [(= "in" (string/trim current-string)) (= (:type (last vector)) :identifier)])
        (recur (rest my-sequence) (str "" current-char) (conj vector {:type :keyword-in}) new-line-number)

        (every? identity [(= "in" (string/trim current-string)) (and (map? (last vector)) (= :each-token (:type (last vector))) (:value (last vector)))])
        (recur (rest my-sequence) (str "" current-char) (conj (pop vector) {:type :identifier :value (:value (last vector))} {:type :each-in-token}) new-line-number)

        (every? identity [(= (:type (last vector)) :keyword-in) (= next-char \%)])
        (let [current-trimmed (string/trim current-string)]
          (if (not (.isBlank ^String current-trimmed))
            (recur (rest my-sequence) "" (conj vector {:type :identifier :value (parse-path current-trimmed)}) new-line-number)
            (recur (rest my-sequence) "" (conj vector {:type :identifier}) new-line-number)))

        (every? identity [(= (:type (last vector)) :each-in-token) (= next-char \%)])
        (let [current-trimmed (string/trim current-string)]
          (if (not (.isBlank ^String current-trimmed))
            (recur (rest my-sequence) "" (conj vector {:type :each-identifier-token :value (parse-path current-trimmed)}) new-line-number)
            (recur (rest my-sequence) "" (conj vector {:type :each-identifier-token}) new-line-number)))

        (every? identity [(= (:type (last vector)) :keyword-in)])
        (recur (rest my-sequence) (str current-string current-char) vector new-line-number)

        (every? identity [(= (:type (last vector)) :each-in-token)])
        (recur (rest my-sequence) (str current-string current-char) vector new-line-number)

        (and (= current-char \}) (= next-char \}))
        (let [trimmed-string (string/trim current-string)
              last-type (:type (last vector))]
          (cond
            (or (= last-type :filter-tag)
                (= last-type :filter-function)
                (= last-type :filter-arg))
            (if (not (string/blank? trimmed-string))
              (let [token-type (if (= last-type :filter-tag)
                                 :filter-function
                                 :filter-arg)]
                (recur (rrest my-sequence) "" (conj vector {:type token-type :value (keyword trimmed-string)}
                                                    {:type :closing-bracket :line line-number}) new-line-number))
              (recur (rrest my-sequence) "" (conj vector {:type :closing-bracket :line line-number}) new-line-number))

            (string/blank? trimmed-string)
            (recur (rrest my-sequence) "" (conj vector {:type :expression}
                                                {:type :closing-bracket :line line-number}) new-line-number)

            :else
            (recur (rrest my-sequence) "" (conj vector {:type  :expression
                                                        :value (parse-path trimmed-string)}
                                                {:type :closing-bracket :line line-number}) new-line-number)))

        (= (:type (last vector)) :opening-bracket)
        (cond
          (and (= current-char \|) (not (string/blank? current-string)))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj vector {:type :expression :value (parse-path trimmed-string)} {:type :filter-tag}) new-line-number))
          :else
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (or (= (:type (last vector)) :filter-tag)
            (= (:type (last vector)) :filter-function)
            (= (:type (last vector)) :filter-arg))
        (cond
          (and (= current-char \") (string/blank? (string/trim current-string)))
          (recur (rest my-sequence) "\"" vector new-line-number)

          (and (= current-char \") (string/starts-with? current-string "\""))
          (recur (rest my-sequence) "" (conj vector {:type :filter-arg :value (subs current-string 1)}) new-line-number)

          (and (= current-char \|)
               (not (string/blank? (string/trim current-string)))
               (= (:type (last vector)) :filter-tag))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj vector {:type :filter-function :value (keyword trimmed-string)} {:type :filter-tag}) new-line-number))

          (and (= current-char \|)
               (not (string/blank? (string/trim current-string)))
               (or (= (:type (last vector)) :filter-function)
                   (= (:type (last vector)) :filter-arg)))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj vector {:type :filter-arg :value (keyword trimmed-string)} {:type :filter-tag}) new-line-number))

          (and (= current-char \|) (string/blank? (string/trim current-string)))
          (recur (rest my-sequence) "" (conj vector {:type :filter-tag}) new-line-number)

          (and (= current-char \ )
               (not (string/blank? (string/trim current-string)))
               (not (string/starts-with? current-string "\""))
               (= (:type (last vector)) :filter-tag))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj vector {:type :filter-function :value (keyword trimmed-string)}) new-line-number))

          (and (= current-char \ )
               (not (string/blank? (string/trim current-string)))
               (not (string/starts-with? current-string "\""))
               (or (= (:type (last vector)) :filter-function)
                   (= (:type (last vector)) :filter-arg)))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj vector {:type :filter-arg :value (keyword trimmed-string)}) new-line-number))

          (and (= current-char \ ) (string/blank? (string/trim current-string)))
          (recur (rest my-sequence) "" vector new-line-number)

          (and (= current-char \ ) (string/starts-with? current-string "\""))
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number)

          (and (not= current-char \") (not= current-char \|) (not= current-char \ ))
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number)

          :else
          (recur (rest my-sequence) current-string vector new-line-number))

        (= (:type (last vector)) :block-start)
        (cond
          (= (string/trim current-string) "for")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-for}) new-line-number)

          (= (string/trim current-string) "each")
          (recur (rest my-sequence) "" (conj vector {:type :each-token}) new-line-number)

          (= (string/trim current-string) "let")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-let}) new-line-number)

          (= (string/trim current-string) "escape")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-escape}) new-line-number)

          (= (string/trim current-string) "query-string")
          (recur (rest my-sequence) "" (conj vector {:type :keyword-query-string}) new-line-number)

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

          (= (string/trim current-string) "now")
          (recur (rest my-sequence) "" (conj vector {:type :now}) new-line-number)

          (= (string/trim current-string) "verbatim")
          (recur (rest my-sequence) "" (conj vector {:type :verbatim}) new-line-number)

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
          (recur (rest my-sequence) (str "" current-char) (conj vector {:type :identifier :value (parse-path (string/trim current-string))}) new-line-number)
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :keyword-let)
        (if (= next-char \%)
          (let [{:keys [variable-name variable-value]} (parse-let-assignment (string/trim current-string))]

            (recur (rest my-sequence) "" (conj vector {:type           :variable-declaration
                                                       :variable-name  variable-name
                                                       :variable-value variable-value}) new-line-number))
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :keyword-query-string)
        (if (= next-char \%)
          (let [variable-value (parse-query-string-value current-string)]
            (recur (rest my-sequence) "" (conj vector {:type           :query-string-declaration
                                                       :variable-value variable-value})
                   new-line-number))
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (or (= (:type (last vector)) :now)
            (and (map? (last vector)) (:now-format (last vector))))
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string vector new-line-number)

          (and (string/blank? current-string) (= current-char \"))
          (recur (rest my-sequence) "" vector new-line-number)

          (and (not (string/blank? current-string)) (= current-char \"))
          (cond
            (= (:type (last vector)) :now)
            (recur (rest my-sequence) "" (conj vector {:now-format current-string}) new-line-number)

            (and (map? (last vector)) (:now-format (last vector)))
            (recur (rest my-sequence) "" (conj vector {:now-timezone current-string}) new-line-number))

          (and (string/blank? current-string) (= current-char \%) (= next-char \}))
          (let [updated-vector (conj (pop vector) {:type :now})]
            (recur (rest my-sequence) (str current-char) updated-vector new-line-number))

          (not (= current-char \"))
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :keyword-for)
        (cond
          (and (not (string/blank? current-string))
               (or (= current-char \ ) (= next-char \%)))
          (recur (rest my-sequence) (str "" current-char) (conj vector {:type :identifier :value (keyword (string/trim current-string))}) new-line-number)
          :else
          (recur (rest my-sequence) (str current-string current-char) vector new-line-number))

        (= (:type (last vector)) :each-token)
        (cond
          (and (not (string/blank? current-string))
               (or (= current-char \ ) (= next-char \%)))
          (recur (rest my-sequence) (str "" current-char) (conj vector {:type :each-token :value (keyword (string/trim current-string))}) new-line-number)
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

    (if-not (empty? current-string)
      (conj vector {:type :text :value current-string})
      vector)))

(defn tokenize [string]
  (tokenize-recursively (seq string) "" [] 1))
