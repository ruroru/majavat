(ns jj.majavat.parser-test
  (:require [clojure.test :refer [are deftest is]]
            [jj.majavat.parser :as parser]
            [jj.majavat.resolver.fs :as fcr]
            [jj.majavat.resolver.resource :as rcr])
  (:import (java.io File)
           (java.nio.file Files)
           (java.time ZoneId)))

(def contentResolver (rcr/->ResourceResolver))


(deftest test-parse-text
  (is (= [{:type  :text
           :value "hello world"}]
         (parser/parse "test-parse-text.html" contentResolver))))

(deftest insert-value
  (is (= [{:type :text :value "hello "}
          {:type :value-node :value [:name]}]
         (parser/parse "insert-value.html" contentResolver))))

(deftest test-parse-child-value
  (is (= [{:type :text :value "hello "}
          {:type :value-node :value [:user :name]}]
         (parser/parse "insert-child-value.html" contentResolver))))

(deftest test-parse-for-loop
  (is (= [{:body       [{:type  :text
                         :value "hello "}
                        {:type  :value-node
                         :value [:world]}]
           :identifier :world
           :source     [:planets]
           :type       :for}]
         (parser/parse "for-loop.html" contentResolver))))

(deftest if-statement
  (is (= [{:type  :text
           :value "hello "}
          {:condition  [:some
                        :condition]
           :type       :if
           :when-false [{:type  :text
                         :value ""}]
           :when-true  [{:type  :text
                         :value "World from "}
                        {:type  :value-node
                         :value [:location]}]}]
         (parser/parse "if-statement.txt" contentResolver))))

(deftest if-else-statement (is (= [{:type :text :value "hello "}
                                   {:when-true  [{:type :text :value "World"}
                                                 {:type :value-node :value [:location]}]
                                    :when-false [{:type :text :value "jj"}
                                                 {:type :value-node :value [:location]}]
                                    :condition  [:some :condition]
                                    :type       :if}]
                                  (parser/parse "if-else-statement.txt" contentResolver))))


(deftest if-else-statement
  (is (= [{:type  :text
           :value "hello "}
          {:condition  [:some
                        :condition]
           :type       :if
           :when-false [{:type  :text
                         :value "jj! "}
                        {:type  :value-node
                         :value [:location]}]
           :when-true  [{:type  :text
                         :value "World! from "}
                        {:type  :value-node
                         :value [:location]}]}]
         (parser/parse "if-else-statement.txt" contentResolver))))


(deftest includes-test
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "includes-test" contentResolver))))


(deftest includes-parent-file
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "subfolder/include" contentResolver))))

(deftest includes-from-subfolder
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "subfolder/include-subfolder" (rcr/->ResourceResolver)))))


(deftest includes-complicated-path
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "subfolder/include-complicated-subfolder" (rcr/->ResourceResolver)))))

(deftest extends-test
  (is (= [{:type  :text
           :value "this is a header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a  footer"}]
         (parser/parse "extends-file" (rcr/->ResourceResolver)))))

(deftest extends-from-parent-dir
  (is (= [{:type  :text
           :value "this is a header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a  footer"}]
         (parser/parse "subfolder/extends-from-parent-dir" (rcr/->ResourceResolver)))))

(deftest extends-from-sub-dir
  (is (= [{:type  :text
           :value "this is a subfolder header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a subfolder footer"}]
         (parser/parse "subfolder/extends-from-sub-dir" (rcr/->ResourceResolver)))))

(deftest linebreak-parsing
  (.mkdir ^File (File. "./target"))
  (are [expected linebreak-content] (do
                                      (spit "./target/linebreak" linebreak-content)
                                      (= expected (parser/parse "./target/linebreak" (fcr/->FsResolver))))
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
         (parser/parse "if/missing-condition" (rcr/->ResourceResolver)))))


(deftest extends-errors
  (are [expected file-path]
    (= expected
       (parser/parse file-path (rcr/->ResourceResolver)))
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
       (parser/parse file-path (rcr/->ResourceResolver)))
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
                      (parser/parse file-path (rcr/->ResourceResolver)))
                   "loop/for"
                   "loop/for-in"
                   "loop/for-i-in"
                   ))

(deftest if-not-test
  (are [template expected-ast]
    (= expected-ast
       (parser/parse template (rcr/->ResourceResolver)))
    "if/if-not"
    [{:type  :text
      :value "hello "}
     {:condition  [:value]
      :type       :if-not
      :when-false [{:type  :text
                    :value ""}]
      :when-true  [{:type  :text
                    :value "world"}]}]

    "if/if-not-else"
    [{:type  :text
      :value "hello "}
     {:condition  [:value]
      :type       :if-not
      :when-false [{:type  :text
                    :value "universe"}]
      :when-true  [{:type  :text
                    :value "world"}]}]

    "if/if-not-missing-condition"
    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}

    "if/nested-if-not-if"
    [{:type  :text
      :value "start "}
     {:condition  [:flag]
      :type       :if-not
      :when-false [{:type  :text
                    :value ""}]
      :when-true  [{:type  :text
                    :value "middle "}
                   {:condition  [:nested]
                    :type       :if
                    :when-false [{:type  :text
                                  :value ""}]
                    :when-true  [{:type  :text
                                  :value "deep"}]}
                   {:type  :text
                    :value " end"}]}
     {:type  :text
      :value " finish"}]))

(deftest if-not-negative-test
  (are [expected-ast]
    (= expected-ast
       (parser/parse "if/if-not-missing-condition" (rcr/->ResourceResolver)))


    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}))

(deftest test-filters
  (are [expected-ast file-path]
    (= expected-ast
       (parser/parse file-path (rcr/->ResourceResolver)))

    [{:type  :text
      :value "testing "}
     {:filters [{:args        []
                 :filter-name :upper-case}]
      :type    :value-node
      :value   [:value]}] "filter/uppercase"


    [{:type  :text
      :value "foo "}
     {:filters [{:args        []
                 :filter-name :trim}
                {:args        []
                 :filter-name :upper-case}]
      :type    :value-node
      :value   [:value]}]
    "filter/multi-filter"))

(deftest let-test
  (are [expected-ast input-file]
    (= expected-ast
       (parser/parse input-file (rcr/->ResourceResolver)))
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
       (parser/parse input-file (rcr/->ResourceResolver)))
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
       (parser/parse input-file (rcr/->ResourceResolver)))
    [{:type  :text
      :value "/some/route"}
     {:type  :query-string
      :value [:foo :bar]}]
    "query-string/query-string"))


(deftest filter-with-args
  (are [expected-ast input-file]
    (= expected-ast (parser/parse input-file (rcr/->ResourceResolver)))
    [{:filters [{:args        ["arg1"
                               "arg2"]
                 :filter-name :function1}
                {:args        []
                 :filter-name :function2}
                {:args        ["default-value"]
                 :filter-name :default}]
      :type    :value-node
      :value   [:val]}]
    "filter/pipe"))

(deftest now
  (are [expected-ast input-file]
    (= expected-ast (parser/parse input-file (rcr/->ResourceResolver)))
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
    (= expected-ast (parser/parse input-file (rcr/->ResourceResolver)))
    [{:type  :text
      :value "testing "}
     {:type  :text
      :value "foo{{d}}{%d%}{#d#}bar"}] "verbatim/verbatim"))

(deftest filter-without-function
  (are [expected-ast input-file]
    (= expected-ast (parser/parse input-file (rcr/->ResourceResolver)))
    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}
    "filter/empty-filter"
    {:error-message "error on line 3"
     :line          "3"
     :type          "syntax-error"}
    "filter/piped-empty-filter"
    ))

(deftest filter-with-args
  (are [expected-ast input-file]
    (= expected-ast (parser/parse input-file (rcr/->ResourceResolver)))
    [{:type  :text
      :value "testing "}
     {:filters [{:args        ["key1"
                               "value1"]
                 :filter-name :where}
                {:args        []
                 :filter-name :str}]
      :type    :value-node
      :value   [:value]}]
    "filter/where"))