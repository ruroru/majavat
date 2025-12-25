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
          {:type :content}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "hello world {% block  %}"))))


(deftest extends-test
  (is (= [{:type  :text
           :value "testing "}
          {:type :block-start}
          {:type :keyword-extends}
          {:type  :file-path
           :value "file.txt"}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "testing {% extends  \"file.txt\" %}"))))

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
                         {:type :expression :value [:value]}
                         {:type :filter-tag}
                         {:type :filter-function :value :function1}
                         {:line 1 :type :closing-bracket}]
                        "testing {{ value | function1 }}"

                        [{:type  :text
                          :value "testing "}
                         {:type :opening-bracket}
                         {:type :expression :value [:value]}
                         {:type :filter-tag}
                         {:type :filter-function :value :function1}
                         {:type :filter-arg :value "arg1"}
                         {:type :filter-tag}
                         {:type :filter-function :value :function2}
                         {:type :filter-arg :value "arg2"}
                         {:type :filter-arg :value "arg3"}
                         {:type :filter-tag}
                         {:type :filter-function :value :function}
                         {:line 1 :type :closing-bracket}]

                        "testing {{ value |function1 \"arg1\" |           function2         \"arg2\"      \"arg3\"|       function}}"))


(deftest tokenize-let
  (are [expected input] (= expected
                           (lexer/tokenize input))
                        [{:type  :text
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
                        "testing {% let foo = \"bar\" %}hello{% endlet %}"
                        [{:type  :text
                          :value "testing "}
                         {:type :block-start}
                         {:type :keyword-let}
                         {:type           :variable-declaration
                          :variable-name  :foo
                          :variable-value [:bar :qux]}
                         {:line 1
                          :type :block-end}
                         {:type  :text
                          :value "hello"}
                         {:type :block-start}
                         {:type :keyword-end-let}
                         {:line 1
                          :type :block-end}
                         ]
                        "testing {% let foo = bar.qux %}hello{% endlet %}"))

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

(deftest csrf-lex-test
  (is (= [{:type :block-start}
          {:type :keyword-csrf-token}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% csrf-token %}"))))

(deftest query-string
  (is (= [{:type :block-start}
          {:type :keyword-query-string}
          {:type           :query-string-declaration
           :variable-value [:bar :qux]}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% query-string bar.qux %}"))))

(deftest now
  (are [expected input] (= expected (lexer/tokenize input))
                        [{:type  :text
                          :value "current time is "}
                         {:type :block-start}
                         {:type :now}

                         {:line 1
                          :type :block-end}]
                        "current time is {% now %}"
                        [{:type  :text
                          :value "current time is "}
                         {:type :block-start}
                         {:type :now}
                         {:now-format "yyyy-MM-dd/hh:mm"}
                         {:line 1
                          :type :block-end}]
                        "current time is {% now \"yyyy-MM-dd/hh:mm\" %}"
                        [{:type  :text
                          :value "current time is "}
                         {:type :block-start}
                         {:type :now}
                         {:now-format "yyyy-MM-dd/hh:mm"}
                         {:now-timezone "Asia/Tokyo"}
                         {:line 1 :type :block-end}]
                        "current time is {% now \"yyyy-MM-dd/hh:mm\" \"Asia/Tokyo\" %}"))

(deftest verbatim-tokenize
  (are [input expected] (= expected (lexer/tokenize input))
                        "testing {% verbatim %}foo{{}}{%%}{##}bar{% endverbatim %}"
                        [{:type :text :value "testing "}
                         {:type :block-start}
                         {:type :verbatim}
                         {:line 1 :type :block-end}
                         {:type :text :value "foo{{}}{%%}{##}bar"}
                         {:type :block-start}
                         {:type :end-verbatim}
                         {:line 1 :type :block-end}]
                        "testing {% verbatim %}  foo{{}}{%%}{##}bar  {% endverbatim %}"
                        [{:type :text :value "testing "}
                         {:type :block-start}
                         {:type :verbatim}
                         {:line 1 :type :block-end}
                         {:type :text :value "  foo{{}}{%%}{##}bar  "}
                         {:type :block-start}
                         {:type :end-verbatim}
                         {:line 1 :type :block-end}]))

(deftest tokenize-keyword-arguments
  (let [expected [{:type  :text
                   :value "testing "}
                  {:type :opening-bracket}
                  {:type :expression :value [:value]}
                  {:type :filter-tag}
                  {:type :filter-function :value :function1}
                  {:type :filter-arg :value :arg}
                  {:type :filter-tag}
                  {:type :filter-function :value :function2}
                  {:type :filter-arg :value :foo}
                  {:type :filter-arg :value :bar}
                  {:type :filter-arg :value :baz}
                  {:type :filter-tag}
                  {:type :filter-function :value :func3}
                  {:type :filter-arg :value :qaz}
                  {:type :filter-arg :value :quux}
                  {:line 1 :type :closing-bracket}]
        input "testing {{ value |function1 arg |     function2   foo bar baz|func3 qaz   quux   }}"]
    (is (= expected (lexer/tokenize input)))))

(deftest tokenize-keyword-arguments-with-space
  (let [expected [{:type  :text
                   :value "testing "}
                  {:type :opening-bracket}
                  {:type :expression :value [:value]}
                  {:type :filter-tag}
                  {:type :filter-function :value :function}
                  {:type :filter-arg :value "foo bar"}
                  {:type :filter-arg :value :baz}
                  {:line 1 :type :closing-bracket}]
        input "testing {{ value | function   \"foo bar\" baz}}"]
    (is (= expected (lexer/tokenize input)))))

(deftest tokenize-nil
  (let [expected []]
    (is (= expected (lexer/tokenize nil)))))


(deftest each-test
  (is (= [{:type :block-start}
          {:type :each-token}
          {:type  :identifier
           :value :world}
          {:type :each-in-token}
          {:type  :each-identifier-token
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
          {:type :end-each-token}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% each world in planets %} hello {{ world }}{% endeach %}"))))