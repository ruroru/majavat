(ns jj.majavat.parser-test
  (:require [clojure.test :refer [are deftest is]]
            [clojure.walk :as walk]
            [jj.majavat.lexer :as lexer]
            [jj.majavat.parser :as parser]
            [clojure.pprint :as pprint]
            [jj.majavat.renderer.tests :as tests]
            [jj.majavat.renderer.sanitizer :refer [->None]]
            [jj.majavat.renderer.json :refer [->DefaultJsonSerializer]]
            [jj.majavat.protocol.mock-dictionary :refer [create-mock-dictionary]]
            [jj.majavat.resolver.fs :as fcr]
            [jj.majavat.protocol.dictionary :as dictionary]
            [jj.majavat.resolver.resource :as rcr])
  (:import (java.io File)))

(def contentResolver (rcr/->ResourceResolver))
(def empty-fn-map {})
(def empty-sanitizers-map {})
(def ^:private default-dictionary (create-mock-dictionary))
(def ^:private default-sanitizer (->None))
(def ^:private default-json-serializer (->DefaultJsonSerializer))

(defn- parse [& args]
  (let [result (apply parser/parse
                      (concat args (drop (- (count args) 4)
                                         [default-dictionary default-sanitizer default-json-serializer])))]
    (walk/postwalk #(if (map? %) (dissoc % :render-fn) %)
                   (if (map? result) result (first result)))))


(deftest test-parse-text
  (is (= [{:type  :text
           :value "hello world"}]
         (parse "test-parse-text.html" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest insert-value
  (is (= [{:type :text :value "hello "}
          {:type :value-node}]
         (parse "insert-value.html" contentResolver empty-fn-map empty-sanitizers-map)))
  (let [value-node (second (first (parser/parse "insert-value.html" contentResolver empty-fn-map empty-sanitizers-map default-dictionary default-sanitizer default-json-serializer)))]
    (is (= "world" ((:render-fn value-node) {:name "world"})))))

(deftest test-parse-child-value
  (is (= [{:type :text :value "hello "}
          {:type :value-node}]
         (parse "insert-child-value.html" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest test-parse-for-loop
  (is (= [{:body       [{:type  :text
                         :value "hello "}
                        {:type :value-node}]
           :identifier :world
           :source     [:planets]
           :type       :for}]
         (parse "for-loop.html" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest test-parse-for-loop-else
  (is (= [{:type  :text
           :value "The planets are: "}
          {:body       [{:type :value-node}]
           :identifier :world
           :source     [:planets]
           :type       :for
           :when-empty [{:type  :text
                         :value "No planets"}]}]
         (parse "loop/for-loop-else" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest if-statement
  (is (= [{:type  :text
           :value "hello "}
          {:branches [[{:condition           [:some
                                              :condition]
                        :evaluation-function tests/default-test}
                       [{:type  :text
                         :value "World from "}
                        {:type :value-node}]]]
           :else     []
           :type     :if}]
         (parse "if-statement.txt" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest if-else-statement
  (is (= [{:type  :text
           :value "hello "}
          {:branches [[{:condition           [:some
                                              :condition]
                        :evaluation-function tests/default-test}
                       [{:type  :text
                         :value "World! from "}
                        {:type :value-node}]]]
           :else     [{:type  :text
                       :value "jj! "}
                      {:type :value-node}]
           :type     :if}]
         (parse "if-else-statement.txt" contentResolver empty-fn-map empty-sanitizers-map))))


(deftest includes-test
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type :value-node}]
         (parse "includes-test" contentResolver empty-fn-map empty-sanitizers-map))))


(deftest includes-parent-file
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type :value-node}]
         (parse "subfolder/include" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest includes-from-subfolder
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type :value-node}]
         (parse "subfolder/include-subfolder" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))


(deftest includes-complicated-path
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type :value-node}]
         (parse "subfolder/include-complicated-subfolder" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))

(deftest extends-test
  (is (= [{:type  :text
           :value "this is a header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a  footer"}]
         (parse "extends-file" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))

(deftest extends-from-parent-dir
  (is (= [{:type  :text
           :value "this is a header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a  footer"}]
         (parse "subfolder/extends-from-parent-dir" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))

(deftest extends-from-sub-dir
  (is (= [{:type  :text
           :value "this is a subfolder header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a subfolder footer"}]
         (parse "subfolder/extends-from-sub-dir" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))

(deftest fragment-node
  (is (= [{:type :text :value "<html>"}
          {:type :fragment
           :name :row
           :line 1
           :body [{:type :text :value "<li>"}
                  {:type :value-node}
                  {:type :text :value "</li>"}]}
          {:type :text :value "<footer>bye</footer></html>"}]
         (parse "fragment/page.html" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest unclosed-fragment-error
  (is (= {:type          "syntax-error"
          :error-message "Unclosed 'fragment' tag starting on line 1"
          :line          "1"}
         (parse "fragment/unclosed.html" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest fragment-missing-name-error
  (is (= "syntax-error"
         (:type (parse "fragment/no-name.html" contentResolver empty-fn-map empty-sanitizers-map)))))

(deftest linebreak-parsing
  (.mkdir ^File (File. "./target"))
  (are [expected linebreak-content] (do
                                      (spit "./target/linebreak" linebreak-content)
                                      (= expected (parse "./target/linebreak" (fcr/->FsResolver) empty-fn-map empty-sanitizers-map)))
                                    [{:type  :text
                                      :value "hello\r\r"}
                                     {:type :value-node}
                                     {:type  :text
                                      :value "\rworld"}]
                                    "hello\r\r{{ name }}\rworld"
                                    [{:type  :text
                                      :value "hello\r\n\r\n"}
                                     {:type :value-node}
                                     {:type  :text
                                      :value "\r\nworld"}]
                                    "hello\r\n\r\n{{ name }}\r\nworld"
                                    {:error-message "error on line 3"
                                     :line          "3"
                                     :type          "syntax-error"}
                                    "hello\n\n{{  }}\nworld"
                                    {:error-message "error on line 3"
                                     :line          "3"
                                     :type          "syntax-error"}
                                    "hello\r\n\r\n{{  }}\r\nworld"
                                    {:error-message "error on line 3"
                                     :line          "3"
                                     :type          "syntax-error"}
                                    "hello\r\r{{  }}\rworld"))


(deftest returns-error-with-line-3-if-missing-condition-in-if
  (is (= {:error-message "error on line 3"
          :line          "3"
          :type          "syntax-error"}
         (parse "if/missing-condition" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))


(deftest extends-errors
  (are [expected file-path]
    (= expected
       (parse file-path (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}
    "extends/contains-only-extends"
    {:error-message "./asdasdasd template can not be found"
     :type          "template-not-found-error"}
    "extends/parent-template-does-not-exist"))

(deftest include-errors
  (are [expected file-path]
    (= expected
       (parse file-path (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}
    "include/missing-file-name"
    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}
    "include/missing-file-name"
    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}
    "include/not-existing-file"
    ))



(deftest faulty-for-loop
  (are [file-path] (= {:error-message "error on line 3"
                       :line          "3"
                       :type          "syntax-error"}
                      (parse file-path (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
                   "loop/for"
                   "loop/for-in"
                   "loop/for-i-in"
                   ))

(deftest if-not-test
  (are [template expected-ast]
    (= expected-ast
       (parse template (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    "if/if-not"
    [{:type  :text
      :value "hello "}
     {:branches [[{:condition           [:value]
                   :evaluation-function tests/default-test
                   :negate              true}
                  [{:type  :text
                    :value "world"}]]]
      :else     []
      :type     :if}]

    "if/if-not-else"
    [{:type  :text
      :value "hello "}
     {:branches [[{:condition           [:value]
                   :evaluation-function tests/default-test
                   :negate              true}
                  [{:type  :text
                    :value "world"}]]]
      :else     [{:type  :text
                  :value "universe"}]
      :type     :if}]

    "if/if-not-missing-condition"
    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}

    "if/nested-if-not-if"
    [{:type  :text
      :value "start "}
     {:branches [[{:condition           [:flag]
                   :evaluation-function tests/default-test
                   :negate              true}
                  [{:type  :text
                    :value "middle "}
                   {:branches [[{:condition           [:nested]
                                 :evaluation-function tests/default-test}
                                [{:type  :text
                                  :value "deep"}]]]
                    :else     []
                    :type     :if}
                   {:type  :text
                    :value " end"}]]]
      :else     []
      :type     :if}
     {:type  :text
      :value " finish"}]))

(deftest if-not-negative-test
  (are [expected-ast]
    (= expected-ast
       (parse "if/if-not-missing-condition" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))


    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}))


(deftest let-test
  (are [expected-ast input-file]
    (= expected-ast
       (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type  :text
      :value "testing "}
     {:type           :variable-declaration
      :variable-name  :foo
      :variable-value "bar"
      :body           [{:type  :text
                        :value "hello "}
                       {:type :value-node}]}
     {:type  :text
      :value "baz"}
     ] "let/let-foo"
    [{:type  :text
      :value "testing "}
     {:type           :variable-assignment
      :variable-name  :foo
      :variable-value [:bar :qux]
      :body           [{:type  :text
                        :value "hello "}
                       {:type :value-node}]}
     {:type  :text
      :value "baz"}
     ]
    "let/let-bar"))


(deftest macro-calls-are-unexpanded
  (are [expected-ast input-file]
    (= expected-ast
       (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))

    [{:type :text :value "foo "}
     {:type :macro-call :name :csrf-token :args [] :line 1}
     {:type :text :value " "}
     {:type :value-node}]
    "csrf/csrf"

    [{:type :text :value "/some/route"}
     {:type :macro-call :name :query-string :args [[:foo :bar]] :line 1}]
    "query-string/query-string"

    [{:type :macro-call :name :greet :args [[:name]] :line 1}
     {:type :macro-call :name :greet :args [[:user :name]] :line 1}]
    "macro/macro-with-arg"))


(deftest now
  (are [expected-ast input-file]
    (= expected-ast (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type  :text
      :value "current time is "}
     {:type :macro-call :name :now :args [] :line 1}]
    "now/now"
    [{:type  :text
      :value "current time is "}
     {:type :macro-call :name :now :args ["yyyy-MM-dd"] :line 1}]
    "now/now-with-format"

    [{:type  :text
      :value "current time is  "}
     {:type :macro-call :name :now :args ["yyyy-MM-dd" "Asia/Tokyo"] :line 1}]
    "now/now-with-format-and-time-zone"))

(deftest verbatim
  (are [expected-ast input-file]
    (= expected-ast (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type  :text
      :value "testing "}
     {:type  :text
      :value "foo{{d}}{%d%}{#d#}bar"}] "verbatim/verbatim"))

(deftest filter-without-function
  (are [expected-ast input-file]
    (= expected-ast (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}
    "filter/empty-filter"
    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}
    "filter/piped-empty-filter"
    ))

(deftest test-parse-for-only-loop
  (is (= [{:body       [{:type  :text
                         :value "hello "}
                        {:type :value-node}]
           :identifier :world
           :source     [:planets]
           :type       :each}]
         (parse "each/each" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest unclosed-tag-error
  (are [expected-ast input-file]
    (= expected-ast (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    {:error-message "Unclosed 'let' tag starting on line 1"
     :line          "1"
     :type          "syntax-error"}
    "tagstack/unclosed-let-tag"

    {:error-message "Mismatched closing tag on line 1: expected 'endif' but found 'endlet' (opening tag was on line 1)"
     :line          "1"
     :type          "syntax-error"}
    "tagstack/unclosed-if-tag"

    {:error-message "Mismatched closing tag on line 1: expected 'endfor' but found 'endlet' (opening tag was on line 1)"
     :line          "1"
     :type          "syntax-error"}
    "tagstack/unclosed-for-tag"

    {:error-message "Mismatched closing tag on line 1: expected 'endeach' but found 'endlet' (opening tag was on line 1)"
     :line          "1"
     :type          "syntax-error"}
    "tagstack/unclosed-each-tag"
    ))


(deftest escape-tag
  (let [result (first (parser/parse "escape/escape-html" contentResolver empty-fn-map empty-sanitizers-map default-dictionary default-sanitizer default-json-serializer))
        node (first result)]
    (is (= [{:type :value-node}]
           (parse "escape/escape-html" contentResolver empty-fn-map empty-sanitizers-map)))
    (is (= "&lt;some&gt;tag&lt;/some&gt;" ((:render-fn node) {:value "<some>tag</some>"})))))


(deftest if-elif
  (are [template expected-ast]
    (= expected-ast
       (parse template (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    "if/if-elif-else"
    [{:branches [[{:evaluation-function tests/default-test
                   :condition           [:small]}
                  [{:type  :text
                    :value "small"}]]
                 [{:condition           [:big]
                   :evaluation-function tests/default-test}
                  [{:type  :text
                    :value "big"}]]]
      :else     [{:type  :text
                  :value "none"}]
      :type     :if}]

    ))

(deftest if-is-even

  (pprint/pprint (lexer/tokenize "{% if value is even %}even{% else %}odd{% endif %}"))

  (are [template expected-ast]
    (= expected-ast
       (parse template (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    "if/if-is-even-else"
    [{:branches [[{:evaluation-function tests/is-even?
                   :condition           [:value]}
                  [{:type  :text
                    :value "even"}]]]
      :else     [{:type  :text
                  :value "odd"}]
      :type     :if}]))

(deftest parsed-debug
  (pprint/pprint (lexer/tokenize "{% debug %}"))
  (are [expected-ast input-file]
    (= expected-ast
       (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type   :debug
      :target :default}]
    "debug/debug"))

(deftest parsed-debug
  (pprint/pprint (lexer/tokenize "{% debug logger %}"))
  (are [expected-ast input-file]
    (= expected-ast
       (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type   :debug
      :target :logger}]
    "debug/debug-with-target"))


(deftest trans-test
  (let [mock-dictionary (create-mock-dictionary)
        input-file "trans/trans"
        result (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map mock-dictionary)
        trans-fn (:trans-fn (first result))]

    (is (= 1 (count result)))
    (is (fn? trans-fn))
    (is (= "hei" (trans-fn "fi")))
    (is (= "hello" (trans-fn "en")))))


(deftest if-is-equals-string
  (let [result (parse "if/if-equals-string" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)
        if-node (first result)
        [condition body] (first (:branches if-node))
        eval-fn (:evaluation-function condition)]
    (is (= :if (:type if-node)))
    (is (= [:some :condition] (:condition condition)))
    (is (= [{:type :text :value "yes"}] body))
    (is (= [] (:else if-node)))
    (is (true? (eval-fn "string")))
    (is (false? (eval-fn "other")))))


(deftest if-is-equals-string
  (let [result (parse "if/if-equals-1" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)
        if-node (first result)
        [condition body] (first (:branches if-node))
        eval-fn (:evaluation-function condition)]
    (is (= :if (:type if-node)))
    (is (= [:some :condition] (:condition condition)))
    (is (= [{:type :text :value "yes"}] body))
    (is (= [] (:else if-node)))
    (is (true? (eval-fn 1)))
    (is (false? (eval-fn "other")))))


(deftest macro-definition-ends-with-comma
  (let [expected-ast {:error-message "error on line 1"
                      :line          "1"
                      :type          "syntax-error"}
        input-file "macro/definition-ends-with-comma"]
    (is (= expected-ast (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))


(deftest import-macro-collision
  (let [expected-ast {:error-message "macro 'greet' is already defined"
                      :line          "2"
                      :type          "syntax-error"}
        input-file "import/collision"]
    (is (= expected-ast (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))


(deftest import-not-existing-file
  (let [expected-ast {:error-message "does-not-exist template can not be found"
                      :type          "template-not-found-error"}
        input-file "import/not-existing-file"]
    (is (= expected-ast (parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))


