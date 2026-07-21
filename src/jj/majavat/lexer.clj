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
            (= c \`)
            (let [close (.indexOf s "`" (int (inc i)))]
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

(defn- macro-value-token [kind trimmed-string]
  (if (= kind :macro-def)
    {:type :macro-param :value (keyword trimmed-string)}
    (cond
      (and (string/starts-with? trimmed-string "\"")
           (string/ends-with? trimmed-string "\"")
           (> (count trimmed-string) 1))
      {:type :macro-arg :value (subs trimmed-string 1 (dec (count trimmed-string)))}

      (re-matches #"-?\d+(\.\d+)?" trimmed-string)
      {:type :macro-arg :value trimmed-string}

      :else
      {:type :macro-arg :value (parse-path trimmed-string)})))

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

(defn- tokenize-recursively [my-sequence current-string stack line-number]
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
            (recur remaining-seq "" stack updated-line-number)
            (recur remaining-seq "" (conj stack {:type :text :value current-string}) updated-line-number)))

        (and (= current-char \{) (= next-char \{))
        (cond
          (empty? current-string)
          (recur (rrest my-sequence) "" (conj stack {:type :opening-bracket}) new-line-number)
          :else
          (recur (rrest my-sequence) "" (conj stack {:type :text :value current-string} {:type :opening-bracket}) new-line-number))

        (and (= current-char \{) (= next-char \%))
        (cond
          (empty? current-string)
          (recur (rrest my-sequence) "" (conj stack {:type :block-start}) new-line-number)
          :else
          (recur (rrest my-sequence) "" (conj stack {:type :text :value current-string} {:type :block-start}) new-line-number))

        (and (= current-char \%) (= next-char \}))
        (let [trimmed-string (string/trim current-string)]
          (cond
            (= "endfor" trimmed-string)
            (let [for-only? (loop [tokens stack
                                   depth 0
                                   saw-only? false]
                              (if (empty? tokens)
                                saw-only?
                                (let [t (:type (first tokens))]
                                  (cond
                                    (or (= t :end-for) (= t :end-each-token))
                                    (recur (rest tokens) (inc depth) saw-only?)

                                    (and (= t :only-token) (zero? depth))
                                    (recur (rest tokens) depth true)

                                    (or (= t :keyword-for) (= t :each-token))
                                    (if (pos? depth)
                                      (recur (rest tokens) (dec depth) saw-only?)
                                      saw-only?)

                                    :else
                                    (recur (rest tokens) depth saw-only?)))))
                  end-type (if for-only? :end-each-token :end-for)]
              (recur (rrest my-sequence) "" (conj stack {:type end-type} {:type :block-end :line line-number}) new-line-number))

            (= "endeach" trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :end-each-token} {:type :block-end :line line-number}) new-line-number)

            (= "endverbatim" trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :end-verbatim} {:type :block-end :line line-number}) new-line-number)

            (= "endlet" trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :keyword-end-let} {:type :block-end :line line-number}) new-line-number)

            (= "endmacro" trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :keyword-end-macro} {:type :block-end :line line-number}) new-line-number)

            (= "endescape" trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :keyword-end-escape} {:type :block-end :line line-number}) new-line-number)

            (= "csrf-token" trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :keyword-csrf-token} {:type :block-end :line line-number}) new-line-number)

            (= "now" trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :now} {:type :block-end :line line-number}) new-line-number)

            (= "endif" trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :keyword-endif} {:type :block-end :line line-number}) new-line-number)

            (= "else" trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :keyword-else} {:type :block-end :line line-number}) new-line-number)

            (= "empty" trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :keyword-empty} {:type :block-end :line line-number}) new-line-number)

            (and (= "" trimmed-string) (= (:type (peek stack)) :keyword-extends))
            (recur (rrest my-sequence) "" (conj stack {:type :block-end :line line-number}) new-line-number)

            (and (= "" trimmed-string) (= (:type (peek stack)) :keyword-block))
            (recur (rrest my-sequence) "" (conj stack {:type :content} {:type :block-end :line line-number}) new-line-number)

            (= (:type (peek stack)) :keyword-let)
            (recur (rrest my-sequence) "" (conj stack {:type :block-end :line line-number}) new-line-number)

            (= (:type (peek stack)) :keyword-escape)
            (recur (rrest my-sequence) "" (conj stack {:type :escape-name :value (keyword trimmed-string)} {:type :block-end :line line-number}) new-line-number)

            (= (:type (peek stack)) :keyword-query-string)
            (recur (rrest my-sequence) "" (conj stack {:type :block-end :line line-number}) new-line-number)

            (= (:type (peek stack)) :now)
            (recur (rrest my-sequence) "" (conj stack {:type :block-end :line line-number}) new-line-number)

            (and (map? (peek stack)) (:now-format (peek stack)))
            (recur (rrest my-sequence) "" (conj stack {:type :block-end :line line-number}) new-line-number)

            (= (:type (peek stack)) :verbatim)
            (let [[remaining-seq updated-line-number verbatim-content] (collect-verbatim-content (rrest my-sequence) new-line-number)]
              (if (empty? verbatim-content)
                (recur remaining-seq "" (conj stack {:type :block-end :line line-number}) updated-line-number)
                (recur remaining-seq "" (conj stack {:type :block-end :line line-number} {:type :text :value verbatim-content}) updated-line-number)))

            (= (:type (peek stack)) :token/debug-target)
            (recur (rrest my-sequence) "" (conj stack {:type :block-end :line line-number}) new-line-number)

            (= (:type (peek stack)) :token/debug)
            (recur (rrest my-sequence) "" (conj stack {:type :block-end :line line-number}) new-line-number)

            (= (:type (peek stack)) :macro-name)
            (recur (rrest my-sequence) "" (conj stack {:type :block-end :line line-number}) new-line-number)

            :else
            (recur (rrest my-sequence) "" (conj stack {:type :block-end :line line-number}) new-line-number)))

        (and (= (:type (peek stack)) :identifier)
             (= (string/trim current-string) "is")
             (= current-char \ ))
        (recur (rest my-sequence) "" (conj stack {:type :operator :value :is}) new-line-number)

        (and (= (:type (peek stack)) :identifier)
             (= (string/trim current-string) "==")
             (= current-char \ ))
        (recur (rest my-sequence) "" (conj stack {:type :operator :value :is} {:type :operator-test :value :equals}) new-line-number)

        (and (= (:type (peek stack)) :identifier)
             (= (string/trim current-string) "only")
             (= current-char \ )
             (= :keyword-for (:type (second stack))))
        (recur (rest my-sequence) "" (conj stack {:type :only-token}) new-line-number)

        (= (:type (peek stack)) :operator)
        (if (= next-char \%)
          (let [trimmed (string/trim current-string)]
            (if (not (string/blank? trimmed))
              (recur (rest my-sequence) "" (conj stack {:type :operator-test :value (keyword trimmed)}) new-line-number)
              (recur (rest my-sequence) "" stack new-line-number)))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (and (= (:type (peek stack)) :operator-test) (= (:value (peek stack)) :equals))
        (cond
          (and (= current-char \ ) (string/blank? current-string))
          (recur (rest my-sequence) "" stack new-line-number)

          (and (= current-char \") (string/blank? current-string))
          (recur (rest my-sequence) "\"" stack new-line-number)

          (and (= current-char \") (string/starts-with? current-string "\""))
          (recur (rest my-sequence) "" (conj stack {:type :reference-objet :value (subs current-string 1)}) new-line-number)

          (and (= next-char \%) (not (string/starts-with? current-string "\"")))
          (let [trimmed (string/trim (str current-string current-char))]
            (if (not (string/blank? trimmed))
              (recur (rest my-sequence) "" (conj stack {:type :comparative :value (Long/parseLong trimmed)}) new-line-number)
              (recur (rest my-sequence) "" stack new-line-number)))

          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (every? identity [(= "in" (string/trim current-string)) (= (:type (peek stack)) :identifier)])
        (recur (rest my-sequence) (str "" current-char) (conj stack {:type :keyword-in}) new-line-number)

        (every? identity [(= "in" (string/trim current-string)) (and (map? (peek stack)) (= :each-token (:type (peek stack))) (:value (peek stack)))])
        (recur (rest my-sequence) (str "" current-char) (conj (pop stack) {:type :identifier :value (:value (peek stack))} {:type :each-in-token}) new-line-number)

        (= (:type (peek stack)) :only-token)
        (cond
          (and (= (string/trim current-string) "in") (= current-char \ ))
          (recur (rest my-sequence) "" stack new-line-number)

          (= next-char \%)
          (let [current-trimmed (string/trim current-string)]
            (if (not (.isBlank ^String current-trimmed))
              (recur (rest my-sequence) "" (conj stack {:type :identifier :value (parse-path current-trimmed)}) new-line-number)
              (recur (rest my-sequence) "" (conj stack {:type :identifier}) new-line-number)))

          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (every? identity [(= (:type (peek stack)) :keyword-in) (= next-char \%)])
        (let [current-trimmed (string/trim current-string)]
          (if (not (.isBlank ^String current-trimmed))
            (recur (rest my-sequence) "" (conj stack {:type :identifier :value (parse-path current-trimmed)}) new-line-number)
            (recur (rest my-sequence) "" (conj stack {:type :identifier}) new-line-number)))

        (every? identity [(= (:type (peek stack)) :each-in-token) (= next-char \%)])
        (let [current-trimmed (string/trim current-string)]
          (if (not (.isBlank ^String current-trimmed))
            (recur (rest my-sequence) "" (conj stack {:type :each-identifier-token :value (parse-path current-trimmed)}) new-line-number)
            (recur (rest my-sequence) "" (conj stack {:type :each-identifier-token}) new-line-number)))

        (every? identity [(= (:type (peek stack)) :keyword-in)])
        (recur (rest my-sequence) (str current-string current-char) stack new-line-number)

        (every? identity [(= (:type (peek stack)) :each-in-token)])
        (recur (rest my-sequence) (str current-string current-char) stack new-line-number)

        (and (= current-char \}) (= next-char \}))
        (let [trimmed-string (string/trim current-string)
              last-type (:type (peek stack))]
          (cond
            (or (= last-type :filter-tag)
                (= last-type :filter-function)
                (= last-type :filter-arg))
            (if (not (string/blank? trimmed-string))
              (let [token-type (if (= last-type :filter-tag)
                                 :filter-function
                                 :filter-arg)]
                (recur (rrest my-sequence) "" (conj stack {:type token-type :value (keyword trimmed-string)}
                                                    {:type :closing-bracket :line line-number}) new-line-number))
              (recur (rrest my-sequence) "" (conj stack {:type :closing-bracket :line line-number}) new-line-number))

            (string/blank? trimmed-string)
            (recur (rrest my-sequence) "" (conj stack {:type :expression}
                                                {:type :closing-bracket :line line-number}) new-line-number)

            :else
            (recur (rrest my-sequence) "" (conj stack {:type  :expression
                                                        :value (parse-path trimmed-string)}
                                                {:type :closing-bracket :line line-number}) new-line-number)))

        (= (:type (peek stack)) :opening-bracket)
        (cond
          (and (= current-char \|) (not (string/blank? current-string)))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj stack {:type :expression :value (parse-path trimmed-string)} {:type :filter-tag}) new-line-number))

          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (or (= (:type (peek stack)) :open-paren)
            (= (:type (peek stack)) :comma))
        (let [kind (if (= (:type (peek stack)) :open-paren)
                     (:kind (peek stack))
                     (if (= (:kind (peek stack)) :macro-param) :macro-def :macro))]
          (cond
            (= current-char \))
            (let [trimmed-string (string/trim current-string)]
              (if (string/blank? trimmed-string)
                (recur (rest my-sequence) "" (conj stack {:type :close-paren :kind kind}) new-line-number)
                (recur (rest my-sequence) "" (conj stack (macro-value-token kind trimmed-string)
                                                    {:type :close-paren :kind kind}) new-line-number)))

            (and (= current-char \,)
                 (even? (count (filter #(= \" %) current-string))))
            (let [trimmed-string (string/trim current-string)
                  comma-kind (if (= kind :macro-def) :macro-param :macro-arg)]
              (if (string/blank? trimmed-string)
                (recur (rest my-sequence) "" (conj stack {:type :comma :kind comma-kind}) new-line-number)
                (recur (rest my-sequence) "" (conj stack (macro-value-token kind trimmed-string)
                                                    {:type :comma :kind comma-kind}) new-line-number)))

            :else
            (recur (rest my-sequence) (str current-string current-char) stack new-line-number)))

        (or (= (:type (peek stack)) :filter-tag)
            (= (:type (peek stack)) :filter-function)
            (= (:type (peek stack)) :filter-arg))
        (cond
          (and (= current-char \") (string/blank? (string/trim current-string)))
          (recur (rest my-sequence) "\"" stack new-line-number)

          (and (= current-char \") (string/starts-with? current-string "\""))
          (recur (rest my-sequence) "" (conj stack {:type :filter-arg :value (subs current-string 1)}) new-line-number)

          (and (= current-char \|)
               (not (string/blank? (string/trim current-string)))
               (= (:type (peek stack)) :filter-tag))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj stack {:type :filter-function :value (keyword trimmed-string)} {:type :filter-tag}) new-line-number))

          (and (= current-char \|)
               (not (string/blank? (string/trim current-string)))
               (or (= (:type (peek stack)) :filter-function)
                   (= (:type (peek stack)) :filter-arg)))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj stack {:type :filter-arg :value (keyword trimmed-string)} {:type :filter-tag}) new-line-number))

          (and (= current-char \|) (string/blank? (string/trim current-string)))
          (recur (rest my-sequence) "" (conj stack {:type :filter-tag}) new-line-number)

          (and (= current-char \ )
               (not (string/blank? (string/trim current-string)))
               (not (string/starts-with? current-string "\""))
               (= (:type (peek stack)) :filter-tag))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj stack {:type :filter-function :value (keyword trimmed-string)}) new-line-number))

          (and (= current-char \ )
               (not (string/blank? (string/trim current-string)))
               (not (string/starts-with? current-string "\""))
               (or (= (:type (peek stack)) :filter-function)
                   (= (:type (peek stack)) :filter-arg)))
          (let [trimmed-string (string/trim current-string)]
            (recur (rest my-sequence) "" (conj stack {:type :filter-arg :value (keyword trimmed-string)}) new-line-number))

          (and (= current-char \ ) (string/blank? (string/trim current-string)))
          (recur (rest my-sequence) "" stack new-line-number)

          (and (= current-char \ ) (string/starts-with? current-string "\""))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number)

          (and (not= current-char \") (not= current-char \|) (not= current-char \ ))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number)

          :else
          (recur (rest my-sequence) current-string stack new-line-number))

        (= (:type (peek stack)) :block-start)
        (if-not (or (= current-char \ )
                    (= current-char \()
                    (= current-char \newline)
                    (= current-char \tab)
                    (= current-char \return))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number)
          (cond
          (= (string/trim current-string) "for")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-for}) new-line-number)

          (= (string/trim current-string) "each")
          (recur (rest my-sequence) "" (conj stack {:type :each-token}) new-line-number)

          (= (string/trim current-string) "let")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-let}) new-line-number)

          (= (string/trim current-string) "escape")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-escape}) new-line-number)

          (= (string/trim current-string) "query-string")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-query-string}) new-line-number)

          (= (string/trim current-string) "debug")
          (recur (rest my-sequence) (str "" current-char) (conj stack {:type :token/debug}) new-line-number)

          (= (string/trim current-string) "if")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-if}) new-line-number)

          (= (string/trim current-string) "elif")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-elif}) new-line-number)

          (= (string/trim current-string) "include")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-include}) new-line-number)

          (= (string/trim current-string) "endif")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-endif}) new-line-number)

          (= (string/trim current-string) "extends")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-extends}) new-line-number)

          (= (string/trim current-string) "block")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-block}) new-line-number)

          (= (string/trim current-string) "now")
          (recur (rest my-sequence) "" (conj stack {:type :now}) new-line-number)

          (= (string/trim current-string) "verbatim")
          (recur (rest my-sequence) "" (conj stack {:type :verbatim}) new-line-number)

          (= (string/trim current-string) "macro")
          (recur (rest my-sequence) "" (conj stack {:type :keyword-macro}) new-line-number)

          (= (string/trim current-string) "trans")
          (recur (rest my-sequence) (str "" current-char) (conj stack {:type :token/translation}) new-line-number)

          (and (= current-char \() (not (string/blank? (string/trim current-string))))
          (recur (rest my-sequence) "" (conj stack {:type :macro-call :value (keyword (string/trim current-string)) :line line-number}
                                              {:type :open-paren :kind :macro}) new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number)))

        (= (:type (peek stack)) :token/translation)
        (if (= next-char \%)
          (let [trimmed (string/trim current-string)]
            (if (string/blank? trimmed)
              (recur (rest my-sequence) "" stack new-line-number)
              (recur (rest my-sequence) "" (conj stack {:type  :token/translation-key
                                                         :value (keyword trimmed)})
                     new-line-number)))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (= (:type (peek stack)) :token/debug)
        (if (= next-char \%)
          (let [trimmed (string/trim current-string)]
            (if (string/blank? trimmed)
              (recur (rest my-sequence) "" stack new-line-number)
              (recur (rest my-sequence) "" (conj stack {:type  :token/debug-target
                                                         :value (keyword trimmed)})
                     new-line-number)))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (and (= (:type (peek stack)) :keyword-if)
             (= (string/trim current-string) "not")
             (= current-char \ ))
        (let [current-stack (pop stack)]
          (recur (rest my-sequence) "" (conj current-stack {:type :keyword-if-not}) new-line-number))

        (and (= (:type (peek stack)) :keyword-elif)
             (= (string/trim current-string) "not")
             (= current-char \ ))
        (let [current-stack (pop stack)]
          (recur (rest my-sequence) "" (conj current-stack {:type :keyword-elif-not}) new-line-number))

        (or (= (:type (peek stack)) :keyword-if)
            (= (:type (peek stack)) :keyword-if-not)
            (= (:type (peek stack)) :keyword-elif)
            (= (:type (peek stack)) :keyword-elif-not))
        (if
          (and (= current-char \ ) (not (string/blank? current-string)))
          (recur (rest my-sequence) (str "" current-char) (conj stack {:type :identifier :value (parse-path (string/trim current-string))}) new-line-number)
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (= (:type (peek stack)) :keyword-let)
        (if (= next-char \%)
          (let [{:keys [variable-name variable-value]} (parse-let-assignment (string/trim current-string))]
            (recur (rest my-sequence) "" (conj stack {:type           :variable-declaration
                                                       :variable-name  variable-name
                                                       :variable-value variable-value}) new-line-number))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (= (:type (peek stack)) :keyword-query-string)
        (if (= next-char \%)
          (let [variable-value (parse-query-string-value current-string)]
            (recur (rest my-sequence) "" (conj stack {:type           :query-string-declaration
                                                       :variable-value variable-value})
                   new-line-number))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (or (= (:type (peek stack)) :now)
            (and (map? (peek stack)) (:now-format (peek stack))))
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string stack new-line-number)

          (and (string/blank? current-string) (= current-char \"))
          (recur (rest my-sequence) "" stack new-line-number)

          (and (not (string/blank? current-string)) (= current-char \"))
          (cond
            (= (:type (peek stack)) :now)
            (recur (rest my-sequence) "" (conj stack {:now-format current-string}) new-line-number)

            (and (map? (peek stack)) (:now-format (peek stack)))
            (recur (rest my-sequence) "" (conj stack {:now-timezone current-string}) new-line-number))

          (and (string/blank? current-string) (= current-char \%) (= next-char \}))
          (let [updated-stack (conj (pop stack) {:type :now})]
            (recur (rest my-sequence) (str current-char) updated-stack new-line-number))

          (not (= current-char \"))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (= (:type (peek stack)) :keyword-macro)
        (cond
          (and (not (string/blank? current-string))
               (= current-char \())
          (recur (rest my-sequence) "" (conj stack {:type :macro-name :value (keyword (string/trim current-string))}
                                              {:type :open-paren :kind :macro-def}) new-line-number)

          (and (not (string/blank? current-string))
               (or (= current-char \ ) (= next-char \%)))
          (recur (rest my-sequence) (str "" current-char) (conj stack {:type :macro-name :value (keyword (string/trim current-string))}) new-line-number)
          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (= (:type (peek stack)) :keyword-for)
        (cond
          (and (not (string/blank? current-string))
               (or (= current-char \ ) (= next-char \%)))
          (recur (rest my-sequence) (str "" current-char) (conj stack {:type :identifier :value (keyword (string/trim current-string))}) new-line-number)
          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (= (:type (peek stack)) :each-token)
        (cond
          (and (not (string/blank? current-string))
               (or (= current-char \ ) (= next-char \%)))
          (recur (rest my-sequence) (str "" current-char) (conj stack {:type :each-token :value (keyword (string/trim current-string))}) new-line-number)
          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (= (:type (peek stack)) :keyword-include)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string stack new-line-number)

          (and (string/blank? current-string) (= current-char \"))
          (recur (rest my-sequence) "" stack new-line-number)

          (and (not (string/blank? current-string)) (= current-char \"))
          (recur (rest my-sequence) "" (conj stack {:type :file-path :value current-string}) new-line-number)

          (not (= current-char \"))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (= (:type (peek stack)) :keyword-extends)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string stack new-line-number)

          (and (string/blank? current-string) (= current-char \"))
          (recur (rest my-sequence) "" stack new-line-number)

          (and (not (string/blank? current-string)) (= current-char \"))
          (recur (rest my-sequence) "" (conj stack {:type :file-path :value current-string}) new-line-number)

          (not (= current-char \"))
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        (= (:type (peek stack)) :keyword-block)
        (cond
          (and (string/blank? current-string) (= current-char \ ))
          (recur (rest my-sequence) current-string stack new-line-number)

          (and (not (string/blank? current-string))
               (or (= current-char \ ) (= next-char \%)))
          (recur (rest my-sequence) (str "" current-char) (conj stack {:type :block-name :value (keyword (string/trim current-string))}) new-line-number)

          :else
          (recur (rest my-sequence) (str current-string current-char) stack new-line-number))

        :else
        (recur (rest my-sequence) (str current-string current-char) stack new-line-number)))

    (if-not (empty? current-string)
      (conj stack {:type :text :value current-string})
      stack)))

(defn tokenize [string]
  (reverse (tokenize-recursively (seq string) "" '() 1)))