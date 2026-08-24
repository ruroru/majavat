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

(deftest import-test
  (is (= [{:type  :text
           :value "testing "}
          {:type :block-start}
          {:type :keyword-import}
          {:type  :file-path
           :value "macros/library"}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "testing {% import \"macros/library\" %}"))))

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

(deftest fragment-test
  (is (= [{:type :text :value "<ul>"}
          {:type :block-start}
          {:type :keyword-fragment}
          {:type :fragment-name :value :row}
          {:line 1 :type :block-end}
          {:type :text :value "<li>"}
          {:type :opening-bracket}
          {:type :expression :value [:item]}
          {:line 1 :type :closing-bracket}
          {:type :text :value "</li>"}
          {:type :block-start}
          {:type :keyword-end-fragment}
          {:line 1 :type :block-end}
          {:type :text :value "</ul>"}]
         (lexer/tokenize "<ul>{% fragment row %}<li>{{ item }}</li>{% endfragment %}</ul>"))))

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
                         {:type :open-paren :kind :filter}
                         {:type :filter-arg :value "arg1"}
                         {:type :close-paren :kind :filter}
                         {:type :filter-tag}
                         {:type :filter-function :value :function2}
                         {:type :open-paren :kind :filter}
                         {:type :filter-arg :value "arg2"}
                         {:type :comma :kind :filter-arg}
                         {:type :filter-arg :value "arg3"}
                         {:type :close-paren :kind :filter}
                         {:type :filter-tag}
                         {:type :filter-function :value :function}
                         {:line 1 :type :closing-bracket}]

                        "testing {{ value |function1(\"arg1\") |           function2(\"arg2\", \"arg3\")|       function}}"))


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
          {:type :macro-call :value :csrf-token :line 1}
          {:type :open-paren :kind :macro}
          {:type :close-paren :kind :macro}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% csrf-token() %}"))))

(deftest query-string
  (is (= [{:type :block-start}
          {:type :macro-call :value :query-string :line 1}
          {:type :open-paren :kind :macro}
          {:type :macro-arg :value [:bar :qux]}
          {:type :close-paren :kind :macro}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% query-string(bar.qux) %}"))))

(deftest now
  (are [expected input] (= expected (lexer/tokenize input))
                        [{:type  :text
                          :value "current time is "}
                         {:type :block-start}
                         {:type :macro-call :value :now :line 1}
                         {:type :open-paren :kind :macro}
                         {:type :close-paren :kind :macro}
                         {:line 1
                          :type :block-end}]
                        "current time is {% now() %}"
                        [{:type  :text
                          :value "current time is "}
                         {:type :block-start}
                         {:type :macro-call :value :now :line 1}
                         {:type :open-paren :kind :macro}
                         {:type :macro-arg :value "yyyy-MM-dd/hh:mm"}
                         {:type :close-paren :kind :macro}
                         {:line 1
                          :type :block-end}]
                        "current time is {% now(\"yyyy-MM-dd/hh:mm\") %}"
                        [{:type  :text
                          :value "current time is "}
                         {:type :block-start}
                         {:type :macro-call :value :now :line 1}
                         {:type :open-paren :kind :macro}
                         {:type :macro-arg :value "yyyy-MM-dd/hh:mm"}
                         {:type :comma :kind :macro-arg}
                         {:type :macro-arg :value "Asia/Tokyo"}
                         {:type :close-paren :kind :macro}
                         {:line 1 :type :block-end}]
                        "current time is {% now(\"yyyy-MM-dd/hh:mm\", \"Asia/Tokyo\") %}"))

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

(deftest whitespace-control-tokenize
  (are [input expected] (= expected (lexer/tokenize input))
                        "a\n  {%- if x -%}\n  b{% endif %}"
                        [{:type :text :value "a"}
                         {:type :block-start}
                         {:type :keyword-if}
                         {:type :identifier :value [:x]}
                         {:line 2 :type :block-end}
                         {:type :text :value "b"}
                         {:type :block-start}
                         {:type :keyword-endif}
                         {:line 3 :type :block-end}]

                        "a\n {{- value -}}\n b"
                        [{:type :text :value "a"}
                         {:type :opening-bracket}
                         {:type :expression :value [:value]}
                         {:line 2 :type :closing-bracket}
                         {:type :text :value "b"}]

                        "a {%- if x %}b{% endif -%} c"
                        [{:type :text :value "a"}
                         {:type :block-start}
                         {:type :keyword-if}
                         {:type :identifier :value [:x]}
                         {:line 1 :type :block-end}
                         {:type :text :value "b"}
                         {:type :block-start}
                         {:type :keyword-endif}
                         {:line 1 :type :block-end}
                         {:type :text :value "c"}]

                        "a\n{#- note -#}\nb"
                        [{:type :text :value "a"}
                         {:type :text :value "b"}]

                        "{{- value }}"
                        [{:type :opening-bracket}
                         {:type :expression :value [:value]}
                         {:line 1 :type :closing-bracket}]

                        "{{- value | upper-case -}}"
                        [{:type :opening-bracket}
                         {:type :expression :value [:value]}
                         {:type :filter-tag}
                         {:type :filter-function :value :upper-case}
                         {:line 1 :type :closing-bracket}]))

(deftest whitespace-control-does-not-reach-into-verbatim
  (is (= [{:type :block-start}
          {:type :verbatim}
          {:line 1 :type :block-end}
          {:type :text :value "\n  {{- x -}}\n"}
          {:type :block-start}
          {:type :end-verbatim}
          {:line 3 :type :block-end}]
         (lexer/tokenize "{% verbatim %}\n  {{- x -}}\n{% endverbatim %}"))))

(deftest whitespace-control-keeps-line-numbers
  (is (= [{:type :text :value "x"}
          {:type :opening-bracket}
          {:type :expression :value [:v]}
          {:line 4 :type :closing-bracket}
          {:type :block-start}
          {:type :keyword-if}
          {:type :identifier :value [:q]}
          {:line 5 :type :block-end}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 6 :type :block-end}
          {:type :opening-bracket}
          {:type :expression}
          {:line 7 :type :closing-bracket}]
         (lexer/tokenize "x\n\n\n{{- v -}}\n{% if q -%}\n{%- endif -%}\n{{  }}"))))

(deftest tokenize-keyword-arguments
  (let [expected [{:type  :text
                   :value "testing "}
                  {:type :opening-bracket}
                  {:type :expression :value [:value]}
                  {:type :filter-tag}
                  {:type :filter-function :value :function1}
                  {:type :open-paren :kind :filter}
                  {:type :filter-arg :value :arg}
                  {:type :close-paren :kind :filter}
                  {:type :filter-tag}
                  {:type :filter-function :value :function2}
                  {:type :open-paren :kind :filter}
                  {:type :filter-arg :value :foo}
                  {:type :comma :kind :filter-arg}
                  {:type :filter-arg :value :bar}
                  {:type :comma :kind :filter-arg}
                  {:type :filter-arg :value :baz}
                  {:type :close-paren :kind :filter}
                  {:type :filter-tag}
                  {:type :filter-function :value :func3}
                  {:type :open-paren :kind :filter}
                  {:type :filter-arg :value :qaz}
                  {:type :comma :kind :filter-arg}
                  {:type :filter-arg :value :quux}
                  {:type :close-paren :kind :filter}
                  {:line 1 :type :closing-bracket}]
        input "testing {{ value |function1(arg) |     function2(foo, bar, baz)|func3(qaz, quux)   }}"]
    (is (= expected (lexer/tokenize input)))))

(deftest tokenize-keyword-arguments-with-space
  (let [expected [{:type  :text
                   :value "testing "}
                  {:type :opening-bracket}
                  {:type :expression :value [:value]}
                  {:type :filter-tag}
                  {:type :filter-function :value :function}
                  {:type :open-paren :kind :filter}
                  {:type :filter-arg :value "foo bar"}
                  {:type :comma :kind :filter-arg}
                  {:type :filter-arg :value :baz}
                  {:type :close-paren :kind :filter}
                  {:line 1 :type :closing-bracket}]
        input "testing {{ value | function(\"foo bar\", baz)}}"]
    (is (= expected (lexer/tokenize input)))))

(deftest tokenize-nil
  (let [expected []]
    (is (= expected (lexer/tokenize nil)))))


(deftest for-only-test
  (is (= [{:type :block-start}
          {:type :keyword-for}
          {:type  :identifier
           :value :world}
          {:type :only-token}
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
          {:type :end-each-token}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{%  for world only in planets %} hello {{ world }}{% endfor %}"))))



(deftest escape-test
  (is (= [{:type :block-start}
          {:type :keyword-escape}
          {:type  :escape-name
           :value :json}
          {:line 1
           :type :block-end}
          {:type :opening-bracket}
          {:type  :expression
           :value [:world]}
          {:line 1
           :type :closing-bracket}
          {:type :block-start}
          {:type :keyword-end-escape}
          {:line 1
           :type :block-end}]

         (lexer/tokenize "{% escape json %}{{ world }}{% endescape %}"))))


(deftest lex-namespaced-values
  (is (= [{:type :opening-bracket}
          {:type  :expression
           :value [:foo
                   :bar
                   :baz/qux.quux
                   :corge.grault]}
          {:line 1
           :type :closing-bracket}]
         (lexer/tokenize "{{foo.bar.`baz/qux.quux`.`corge.grault`}}"))))

(deftest if-with-operator-statement
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:some
                   :condition]}
          {:type  :operator
           :value :is}
          {:type  :operator-test
           :value :even}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "yes"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if some.condition is even %}yes{% endif %}"))))

(deftest debug-lex
  (is (= [{:type :block-start}
          {:type :token/debug}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% debug %}"))))

(deftest debug-lex-with-output
  (is (= [{:type :block-start}
          {:type :token/debug}
          {:type :token/debug-target :value :logger-writer}
          {:line 1 :type :block-end}]
         (lexer/tokenize "{% debug logger-writer %}"))))


(deftest macro
  (is (= [{:type :block-start}
          {:type :keyword-macro}
          {:type :macro-name :value :foo}
          {:line 1 :type :block-end}
          {:type :text :value "bar"}
          {:type :opening-bracket}
          {:type :expression :value [:baz]}
          {:line 1 :type :closing-bracket}
          {:type :block-start}
          {:type :keyword-end-macro}
          {:line 1 :type :block-end}
          {:type :block-start}
          {:type :macro-call :value :foo :line 1}
          {:type :open-paren :kind :macro}
          {:type :close-paren :kind :macro}
          {:line 1 :type :block-end}]
         (lexer/tokenize "{% macro foo %}bar{{baz}}{% endmacro %}{% foo() %}"))))


(deftest macro-with-param
  (is (= [{:type :block-start}
          {:type :keyword-macro}
          {:type :macro-name :value :greet}
          {:type :open-paren :kind :macro-def}
          {:type :macro-param :value :who}
          {:type :close-paren :kind :macro-def}
          {:line 1 :type :block-end}
          {:type :text :value "hello "}
          {:type :opening-bracket}
          {:type :expression :value [:who]}
          {:line 1 :type :closing-bracket}
          {:type :block-start}
          {:type :keyword-end-macro}
          {:line 1 :type :block-end}
          {:type :block-start}
          {:type :macro-call :value :greet :line 1}
          {:type :open-paren :kind :macro}
          {:type :macro-arg :value [:name]}
          {:type :close-paren :kind :macro}
          {:line 1 :type :block-end}]
         (lexer/tokenize "{% macro greet(who) %}hello {{who}}{% endmacro %}{% greet(name) %}"))))

(deftest macro-with-literal-arg
  (is (= [{:type :block-start}
          {:type :keyword-macro}
          {:type :macro-name :value :greet}
          {:type :open-paren :kind :macro-def}
          {:type :macro-param :value :who}
          {:type :close-paren :kind :macro-def}
          {:line 1 :type :block-end}
          {:type :opening-bracket}
          {:type :expression :value [:who]}
          {:line 1 :type :closing-bracket}
          {:type :block-start}
          {:type :keyword-end-macro}
          {:line 1 :type :block-end}
          {:type :block-start}
          {:type :macro-call :value :greet :line 1}
          {:type :open-paren :kind :macro}
          {:type :macro-arg :value "world"}
          {:type :close-paren :kind :macro}
          {:line 1 :type :block-end}]
         (lexer/tokenize "{% macro greet(who) %}{{who}}{% endmacro %}{% greet(\"world\") %}"))))

(deftest trans-test
  (is (= [{:type :block-start}
          {:type :macro-call :value :trans :line 1}
          {:type :open-paren :kind :macro}
          {:type :macro-arg :value [:key]}
          {:type :close-paren :kind :macro}
          {:line 1 :type :block-end}]
         (lexer/tokenize "{% trans(key) %}")))
  (is (= [{:type :block-start}
          {:type :macro-call :value :trans :line 1}
          {:type :open-paren :kind :macro}
          {:type :macro-arg :value "key"}
          {:type :close-paren :kind :macro}
          {:line 1 :type :block-end}]
         (lexer/tokenize "{% trans(\"key\") %}"))))


(deftest if-equals-string
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:some :condition]}
          {:type  :operator
           :value :is}
          {:type :operator-test :value :equals}
          {:type :reference-objet :value "value"}
          {:line 1 :type :block-end}
          {:type  :text
           :value "yes"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if some.condition == \"value\" %}yes{% endif %}"))))



(deftest if-equals-invalid-string
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:some :condition]}
          {:type  :operator
           :value :is}
          {:type :operator-test :value :equals}
          {:type :comparative :value 6}
          {:line 1 :type :block-end}
          {:type  :text
           :value "yes"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if some.condition == 6 %}yes{% endif %}"))))

(deftest macro-definition-ends-with-comma
  (is (= (list {:type :block-start}
               {:type :keyword-macro}
               {:type  :macro-name
                :value :macro-name}
               {:kind :macro-def
                :type :open-paren}
               {:type  :macro-param
                :value :argument}
               {:type :comma
                :kind :macro-param}
               {:kind :macro-def
                :type :close-paren}
               {:line 1
                :type :block-end}
               {:type  :text
                :value "hello "}
               {:type :opening-bracket}
               {:type  :expression
                :value [:argument]}
               {:line 1
                :type :closing-bracket}
               {:type :block-start}
               {:type :keyword-end-macro}
               {:line 1
                :type :block-end})
         (lexer/tokenize "{% macro macro-name(argument,) %}hello {{argument}}{% endmacro %}"))))

(deftest macro-multi-params
  (is (= (list {:type :block-start}
               {:type :keyword-macro}
               {:type  :macro-name
                :value :macro-name}
               {:kind :macro-def
                :type :open-paren}
               {:type  :macro-param
                :value :argument1}
               {:kind :macro-param
                :type :comma}
               {:type  :macro-param
                :value :argument2}
               {:kind :macro-def
                :type :close-paren}
               {:line 1
                :type :block-end}
               {:type  :text
                :value "hello "}
               {:type :opening-bracket}
               {:type  :expression
                :value [:argument]}
               {:line 1
                :type :closing-bracket}
               {:type :block-start}
               {:type :keyword-end-macro}
               {:line 1
                :type :block-end})
         (lexer/tokenize "{% macro macro-name(argument1,argument2) %}hello {{argument}}{% endmacro %}"))))


(deftest if-is-lower-than
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:some :condition]}
          {:type  :operator
           :value :is}
          {:type :operator-test :value :lower}
          {:type :comparative :value 6}
          {:line 1 :type :block-end}
          {:type  :text
           :value "yes"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if some.condition < 6 %}yes{% endif %}"))))

(deftest if-is-lower-or-equal
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:some :condition]}
          {:type  :operator
           :value :is}
          {:type :operator-test :value :lower-or-equal}
          {:type :comparative :value 6}
          {:line 1 :type :block-end}
          {:type  :text
           :value "yes"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if some.condition <= 6 %}yes{% endif %}"))))

(deftest if-is-greater-or-equal
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:some :condition]}
          {:type  :operator
           :value :is}
          {:type :operator-test :value :greater-or-equal}
          {:type :comparative :value 6}
          {:line 1 :type :block-end}
          {:type  :text
           :value "yes"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if some.condition >= 6 %}yes{% endif %}"))))

(deftest if-is-greater-then
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:some :condition]}
          {:type  :operator :value :is}
          {:type :operator-test :value :greater}
          {:type :comparative :value 6}
          {:line 1 :type :block-end}
          {:type  :text
           :value "yes"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if some.condition > 6 %}yes{% endif %}"))))