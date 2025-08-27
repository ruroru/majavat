(ns jj.majavat.lexer-test
  (:require [clojure.test :refer [deftest are is]]
            [jj.majavat.lexer :as lexer]))

(deftest lex-test
  (is (= [{:type  :text
           :value "hello world"}]
         (lexer/tokenize "hello world"))))

(deftest lex-opening-closing-bracket-test
  (is (= [{:type  :text
           :value "hello "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:name]}
          {:line 1
           :type :closing-bracket}
          {:type  :text
           :value ", hello "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:user
                   :name]}
          {:line 1
           :type :closing-bracket}]
         (lexer/tokenize "hello {{ name }}, hello {{ user.name }}"))))



(deftest for-loop-test
  (is (= [{:type :block-start}
          {:type :keyword-for}
          {:type  :identifier
           :value :world}
          {:type :keyword-in}
          {:type  :identifier
           :value [:planets]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value " hello "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:world]}
          {:line 1
           :type :closing-bracket}
          {:type :block-start}
          {:type :end-for}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% for world in planets %} hello {{ world }}{% endfor %}"))))




(deftest if-statement
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:some
                   :condition]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "yes"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if some.condition %}yes{% endif %}"))))


(deftest if-else-statement
  (is (= [{:type  :text
           :value "testing if "}
          {:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:condition]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "yes!"}
          {:type :block-start}
          {:type :keyword-else}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "no!"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "testing if {% if condition %}yes!{% else %}no!{% endif %}"))))

(deftest include-test
  (is (= [{:type  :text
           :value "testing "}
          {:type :block-start}
          {:type :keyword-include}
          {:type  :file-path
           :value "file.txt"}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "testing {% include \"file.txt\" %}"))))

(deftest block-extends-test
  (is (= [{:type  :text
           :value "hello world "}
          {:type :block-start}
          {:type :keyword-block}
          {:type  :block-name
           :value :special-block}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "hello world {% block special-block %}"))))


(deftest extends-test
  (is (= [{:type  :text
           :value "testing "}
          {:type :block-start}
          {:type :keyword-extends}
          {:type  :extends-block-name
           :value :special-block}
          {:type  :file-path
           :value "file.txt"}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "testing {% extends special-block \"file.txt\" %}"))))

(deftest tokenize-without-value
  (is (= [{:type  :text
           :value "testing "}
          {:type :opening-bracket}
          {:type :expression}
          {:line 1
           :type :closing-bracket}]
         (lexer/tokenize "testing {{ }}"))))


(deftest tokenize-value-with-filter
  (are [expected input] (= expected (lexer/tokenize input))
                        [{:type  :text
                          :value "testing "}
                         {:type :opening-bracket}
                         {:type  :expression
                          :value [:value]}
                         {:type :filter-tag}
                         {:type  :filter-function
                          :value :upper-case}
                         {:line 1
                          :type :closing-bracket}]
                        "testing {{ value | upper-case }}"

                        [{:type  :text
                          :value "testing "}
                         {:type :opening-bracket}
                         {:type  :expression
                          :value [:value]}
                         {:type :filter-tag}
                         {:type  :filter-function
                          :value :trim}
                         {:type :filter-tag}
                         {:type  :filter-function
                          :value :upper-case}
                         {:line 1
                          :type :closing-bracket}]

                        "testing {{ value | trim | upper-case }}"))


(deftest tokenize-let
  (is (= [{:type  :text
           :value "testing "}
          {:type :block-start}
          {:type :keyword-let}
          {:type           :variable-declaration
           :variable-name  :foo
           :variable-value "bar"}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "hello"}
          {:type :block-start}
          {:type :keyword-end-let}
          {:line 1
           :type :block-end}
          ]
         (lexer/tokenize "testing {% let foo = \"bar\" %}hello{% endlet %}"))))

(deftest tokenize-with-comment
  (is (= [{:type  :text
           :value "testing "}]
         (lexer/tokenize "testing {# this isa {% if value %} {{value}}  comment, and it will not be included #}"))))

(deftest new-line-test
  (is (= [{:type  :text
           :value " "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:value]}
          {:line 1
           :type :closing-bracket}
          {:type  :text
           :value " "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:value]}
          {:line 1
           :type :closing-bracket}
          {:type  :text
           :value " \n\n "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:value]}
          {:line 3
           :type :closing-bracket}
          {:type  :text
           :value " "}]
         (lexer/tokenize " {{ value }} {{ value }} \n\n {{ value }} "))))
