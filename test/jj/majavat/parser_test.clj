(ns jj.majavat.parser-test
  (:require [clojure.test :refer [are deftest is]]
            [jj.majavat.lexer :as lexer]
            [jj.majavat.parser :as parser]
            [clojure.pprint :as pprint]
            [jj.majavat.renderer.tests :as tests]
            [jj.majavat.resolver.fs :as fcr]
            [jj.majavat.protocol.dictionary :as dictionary]
            [jj.majavat.resolver.resource :as rcr])
  (:import (java.io File)
           (java.time ZoneId)
           (jj.majavat.protocol.dictionary Dictionary)))

(def contentResolver (rcr/->ResourceResolver))
(def empty-fn-map {})
(def empty-sanitizers-map {})


(deftest test-parse-text
  (is (= [{:type  :text
           :value "hello world"}]
         (parser/parse "test-parse-text.html" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest insert-value
  (is (= [{:type :text :value "hello "}
          {:type :value-node :value [:name]}]
         (parser/parse "insert-value.html" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest test-parse-child-value
  (is (= [{:type :text :value "hello "}
          {:type :value-node :value [:user :name]}]
         (parser/parse "insert-child-value.html" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest test-parse-for-loop
  (is (= [{:body       [{:type  :text
                         :value "hello "}
                        {:type  :value-node
                         :value [:world]}]
           :identifier :world
           :source     [:planets]
           :type       :for}]
         (parser/parse "for-loop.html" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest test-parse-for-loop-else
  (is (= [{:type  :text
           :value "The planets are: "}
          {:body       [{:type  :value-node
                         :value [:world]}]
           :identifier :world
           :source     [:planets]
           :type       :for
           :when-empty [{:type  :text
                         :value "No planets"}]}]
         (parser/parse "loop/for-loop-else" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest if-statement
  (is (= [{:type  :text
           :value "hello "}
          {:branches [[{:condition           [:some
                                              :condition]
                        :evaluation-function tests/default-test}
                       [{:type  :text
                         :value "World from "}
                        {:type  :value-node
                         :value [:location]}]]]
           :else     []
           :type     :if}]
         (parser/parse "if-statement.txt" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest if-else-statement
  (is (= [{:type  :text
           :value "hello "}
          {:branches [[{:condition           [:some
                                              :condition]
                        :evaluation-function tests/default-test}
                       [{:type  :text
                         :value "World! from "}
                        {:type  :value-node
                         :value [:location]}]]]
           :else     [{:type  :text
                       :value "jj! "}
                      {:type  :value-node
                       :value [:location]}]
           :type     :if}]
         (parser/parse "if-else-statement.txt" contentResolver empty-fn-map empty-sanitizers-map))))


(deftest includes-test
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "includes-test" contentResolver empty-fn-map empty-sanitizers-map))))


(deftest includes-parent-file
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "subfolder/include" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest includes-from-subfolder
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "subfolder/include-subfolder" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))


(deftest includes-complicated-path
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "subfolder/include-complicated-subfolder" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))

(deftest extends-test
  (is (= [{:type  :text
           :value "this is a header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a  footer"}]
         (parser/parse "extends-file" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))

(deftest extends-from-parent-dir
  (is (= [{:type  :text
           :value "this is a header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a  footer"}]
         (parser/parse "subfolder/extends-from-parent-dir" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))

(deftest extends-from-sub-dir
  (is (= [{:type  :text
           :value "this is a subfolder header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a subfolder footer"}]
         (parser/parse "subfolder/extends-from-sub-dir" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))

(deftest linebreak-parsing
  (.mkdir ^File (File. "./target"))
  (are [expected linebreak-content] (do
                                      (spit "./target/linebreak" linebreak-content)
                                      (= expected (parser/parse "./target/linebreak" (fcr/->FsResolver) empty-fn-map empty-sanitizers-map)))
                                    [{:type  :text
                                      :value "hello\r\r"}
                                     {:type  :value-node
                                      :value [:name]}
                                     {:type  :text
                                      :value "\rworld"}]
                                    "hello\r\r{{ name }}\rworld"
                                    [{:type  :text
                                      :value "hello\r\n\r\n"}
                                     {:type  :value-node
                                      :value [:name]}
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
         (parser/parse "if/missing-condition" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))


(deftest extends-errors
  (are [expected file-path]
    (= expected
       (parser/parse file-path (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
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
       (parser/parse file-path (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
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
                      (parser/parse file-path (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
                   "loop/for"
                   "loop/for-in"
                   "loop/for-i-in"
                   ))

(deftest if-not-test
  (are [template expected-ast]
    (= expected-ast
       (parser/parse template (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
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
       (parser/parse "if/if-not-missing-condition" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))


    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}))


(deftest let-test
  (are [expected-ast input-file]
    (= expected-ast
       (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type  :text
      :value "testing "}
     {:type           :variable-declaration
      :variable-name  :foo
      :variable-value "bar"
      :body           [{:type  :text
                        :value "hello "}
                       {:type :value-node :value [:foo]}]}
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
                       {:type :value-node :value [:foo]}]}
     {:type  :text
      :value "baz"}
     ]
    "let/let-bar"))

(deftest csrf-token-test
  (are [expected-ast input-file]
    (= expected-ast
       (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type  :text
      :value "foo "}
     {:type  :text
      :value "<input type=\"hidden\" name=\"csrf_token\" value=\""}
     {:type  :value-node
      :value [:csrf-token]}
     {:type  :text
      :value "\">"}
     {:type  :text
      :value " "}
     {:type  :value-node
      :value [:foo]}]
    "csrf/csrf"))

(deftest query-string
  (are [expected-ast input-file]
    (= expected-ast
       (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type  :text
      :value "/some/route"}
     {:type  :query-string
      :value [:foo :bar]}]
    "query-string/query-string"))


(deftest now
  (are [expected-ast input-file]
    (= expected-ast (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type  :text
      :value "current time is "}
     {:format    "yyyy/MM/dd hh:mm"
      :type      :keyword-now
      :time-zone (.toString ^ZoneId (ZoneId/systemDefault))}]
    "now/now"
    [{:type  :text
      :value "current time is "}
     {:format    "yyyy-MM-dd"
      :time-zone (.toString ^ZoneId (ZoneId/systemDefault))
      :type      :keyword-now}]
    "now/now-with-format"

    [{:type  :text
      :value "current time is  "}
     {:format    "yyyy-MM-dd"
      :time-zone "Asia/Tokyo"
      :type      :keyword-now}]
    "now/now-with-format-and-time-zone"))

(deftest verbatim
  (are [expected-ast input-file]
    (= expected-ast (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type  :text
      :value "testing "}
     {:type  :text
      :value "foo{{d}}{%d%}{#d#}bar"}] "verbatim/verbatim"))

(deftest filter-without-function
  (are [expected-ast input-file]
    (= expected-ast (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
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
                        {:type  :value-node
                         :value [:world]}]
           :identifier :world
           :source     [:planets]
           :type       :each}]
         (parser/parse "each/each" contentResolver empty-fn-map empty-sanitizers-map))))

(deftest unclosed-tag-error
  (are [expected-ast input-file]
    (= expected-ast (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
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
  (is (= [{:body      [{:type  :value-node
                        :value [:value]}]
           :sanitizer #jj.majavat.renderer.sanitizer.Html{}
           :type      :escape-block}]
         (parser/parse "escape/escape-html" contentResolver empty-fn-map empty-sanitizers-map))))


(deftest if-elif
  (are [template expected-ast]
    (= expected-ast
       (parser/parse template (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
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
       (parser/parse template (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
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
       (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type   :debug
      :target :default}]
    "debug/debug"))

(deftest parsed-debug
  (pprint/pprint (lexer/tokenize "{% debug logger %}"))
  (are [expected-ast input-file]
    (= expected-ast
       (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    [{:type   :debug
      :target :logger}]
    "debug/debug-with-target"))


(deftest macro
  (let [expected-ast [{:type :text :value "bar"}
                      {:type :value-node :value [:baz]}
                      {:type :text :value "bar"}
                      {:type :value-node :value [:baz]}]
        input-file "macro/macro"]
    (is (= expected-ast (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))

(deftest macro-open-paren
  (let [expected-ast [{:type :text :value "bar"}
                      {:type :value-node :value [:baz]}
                      {:type :text :value "bar"}
                      {:type :value-node :value [:baz]}]
        input-file "macro/macro-open-paren"]
    (is (= expected-ast (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))

(deftest unknown-block-tag-is-error
  (let [result (parser/parse "macro/macro-unknown" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)]
    (is (= "syntax-error" (:type result)))
    (is (= "unknown tag or macro 'bogus' on line 1" (:error-message result)))))

(deftest macro-with-argument
  (let [expected-ast [{:type :text :value "hello "}
                      {:type :value-node :value [:name]}
                      {:type :text :value "!"}
                      {:type :text :value "hello "}
                      {:type :value-node :value [:user :name]}
                      {:type :text :value "!"}]
        input-file "macro/macro-with-arg"]
    (is (= expected-ast (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))

(deftest macro-with-literal-argument
  (let [expected-ast [{:type :text :value "hello "}
                      {:type :text :value "world"}
                      {:type :text :value "!"}]
        input-file "macro/macro-with-literal-arg"]
    (is (= expected-ast (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))



(defrecord MockDictionary [translations]
  Dictionary
  (translate [_ language word]
    (get-in translations [language word])))

(defn create-mock-dictionary []
  (->MockDictionary {"en" {:hello "hello" :world "world" :key "key"}
                     "fi" {:hello "hei" :world "maailma" :key "avain"}}))


(deftest trans-test
  (let [mock-dictionary (create-mock-dictionary)
        input-file "trans/trans"
        result (parser/parse input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map mock-dictionary)
        trans-fn (:trans-fn (first result))]

    (is (= 1 (count result)))
    (is (fn? trans-fn))
    (is (= "hei" (trans-fn "fi")))
    (is (= "hello" (trans-fn "en")))))


(deftest if-is-equals-string
  (let [result (parser/parse "if/if-equals-string" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)
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
  (let [result (parser/parse "if/if-equals-1" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)
        if-node (first result)
        [condition body] (first (:branches if-node))
        eval-fn (:evaluation-function condition)]
    (is (= :if (:type if-node)))
    (is (= [:some :condition] (:condition condition)))
    (is (= [{:type :text :value "yes"}] body))
    (is (= [] (:else if-node)))
    (is (true? (eval-fn 1)))
    (is (false? (eval-fn "other")))))