(ns jj.majavat.parser-test
  (:require [clojure.test :refer [deftest is are]]
            [jj.majavat.parser :as parser]
            [jj.majavat.resolver.resource :as rcr]
            [mock-clj.core :as mock]))

(def contentResolver (rcr/->ResourceContentResolver))


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
         (parser/parse "subfolder/include-subfolder" (rcr/->ResourceContentResolver)))))


(deftest includes-complicated-path
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "subfolder/include-complicated-subfolder" (rcr/->ResourceContentResolver)))))

(deftest extends-test
  (is (= [{:type  :text
           :value "this is a header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a  footer"}]
         (parser/parse "extends-file" (rcr/->ResourceContentResolver)))))

(deftest extends-from-parent-dir
  (is (= [{:type  :text
           :value "this is a header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a  footer"}]
         (parser/parse "subfolder/extends-from-parent-dir" (rcr/->ResourceContentResolver)))))

(deftest extends-from-sub-dir
  (is (= [{:type  :text
           :value "this is a subfolder header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a subfolder footer"}]
         (parser/parse "subfolder/extends-from-sub-dir" (rcr/->ResourceContentResolver)))))

(deftest returns-error-on-line-3-if-missing-expression
  (is (= {:error-message "error on line 3"
          :line          "3"
          :type          "syntax-error"}
         (mock/with-mock
           [slurp "hello\n\n{{  }}\nworld"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))

(deftest returns-error-on-line-3-if-for-return
  (is (= {:error-message "error on line 3"
          :line          "3"
          :type          "syntax-error"}
         (mock/with-mock
           [slurp "hello\r\r{{  }}\rworld"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))

(deftest returns-error-on-line-3-for-crlf
  (is (= {:error-message "error on line 3"
          :line          "3"
          :type          "syntax-error"}
         (mock/with-mock
           [slurp "hello\r\n\r\n{{  }}\r\nworld"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))

(deftest successful-crlf-parse
  (is (= [{:type  :text
           :value "hello\r\n\r\n"}
          {:type  :value-node
           :value [:name]}
          {:type  :text
           :value "\r\nworld"}]
         (mock/with-mock
           [slurp "hello\r\n\r\n{{ name }}\r\nworld"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))

(deftest successful-cr-parse
  (is (= [{:type  :text
           :value "hello\r\r"}
          {:type  :value-node
           :value [:name]}
          {:type  :text
           :value "\rworld"}]
         (mock/with-mock
           [slurp "hello\r\r{{ name }}\rworld"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))


(deftest returns-error-with-line-3-if-missing-condition-in-if
  (is (= {:error-message "error on line 3"
          :line          "3"
          :type          "syntax-error"}
         (mock/with-mock
           [slurp "hello\n\n{% if %}\nworld{% endif %}"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))


(deftest returns-error-with-line-3-if-missing-block-name
  (is (= {:error-message "error on line 3"
          :line          "3"
          :type          "syntax-error"}
         (mock/with-mock
           [slurp "hello\n\n{% extends %}\nworld"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))

(deftest returns-error-with-line-3-if-missing-file-name
  (is (= {:error-message "error on line 3"
          :line          "3"
          :type          "syntax-error"}
         (mock/with-mock
           [slurp "hello\n\n{% extends block-name %}}"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))


(deftest returns-not-existing-file-error
  (is (= {:error-message "./asdasdasd template can not be found"
          :type          "template-not-found-error"}
         (mock/with-mock
           [slurp "hello\n\n{% extends blockname1 \"./asdasdasd\" %}"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))

(deftest returns-error-when-include-file-is-not-defined
  (is (= {:error-message "error on line 3"
          :line          "3"
          :type          "syntax-error"}
         (mock/with-mock
           [slurp "hello\n\n{% include  %}"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))


(deftest returns-error-when-fail-to-include-not-existing-file
  (is (= {:error-message "not-existing-file.txt template can not be found"
          :type          "template-not-found-error"}
         (mock/with-mock
           [slurp "hello\n\n{% include \"not-existing-file.txt\" %}"]
           (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))))

(deftest faulty-for-loop
  (are [input] (= {:error-message "error on line 3"
                   :line          "3"
                   :type          "syntax-error"}
                  (mock/with-mock
                    [slurp input]
                    (parser/parse "faulty-value" (rcr/->ResourceContentResolver))))
               ;"hello\n\n{% for   %}"
               "hello\n\n{% for in  %}"
               ;"hello\n\n{%  for i in   %} "
               ))
