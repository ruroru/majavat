(ns parser.parser-test
  (:require [clojure.test :refer [deftest is]]
            [jj.majavat.parser :as parser]))


(deftest test-parse-text
  (is (= [{:type  :text
           :value "hello world"}]
         (parser/parse "test-parse-text.html"))))

(deftest insert-value
  (is (= [{:type :text :value "hello "}
          {:type :value-node :value [:name]}]
         (parser/parse "insert-value.html"))))

(deftest test-parse-child-value
  (is (= [{:type :text :value "hello "}
          {:type :value-node :value [:user :name]}]
         (parser/parse "insert-child-value.html"
                       ))))

(deftest test-parse-for-loop
  (is (= [{:body       [{:type  :text
                         :value "hello "}
                        {:type  :value-node
                         :value [:world]}]
           :identifier :world
           :source     [:planets]
           :type       :for}]
         (parser/parse "for-loop.html"))))

(deftest if-statement
  (is (= [{:type :text :value "hello "}
          {:when-true  {:type :text :value "World"}
           :when-false {:type :text :value ""}
           :condition  [:some :condition]
           :type       :if}]
         (parser/parse "if-statement.txt"))))

(deftest if-else-statement (is (= [{:type :text :value "hello "}
                                   {:when-true  {:type :text :value "World!"}
                                    :when-false {:type :text :value "jj!"}
                                    :condition  [:some :condition]
                                    :type       :if}]
                                  (parser/parse "if-else-statement.txt"))))


(deftest if-else-statement
  (is (= [{:type :text :value "hello "}
          {:when-true  {:type :text :value "World!"}
           :when-false {:type :text :value "jj!"}
           :condition  [:some :condition]
           :type       :if}]
         (parser/parse "if-else-statement.txt"))))


(deftest includes-test
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "includes-test"))))


(deftest includes-parent-file
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "subfolder/include"))))

(deftest includes-from-subfolder
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "subfolder/include-subfolder"))))


(deftest includes-complicated-path
  (is (= [{:type  :text
           :value "included content is: "}
          {:type  :text
           :value "hello "}
          {:type  :value-node
           :value [:name]}]
         (parser/parse "subfolder/include-complicated-subfolder"))))

(deftest extends-test
  (is (= [{:type  :text
           :value "this is a header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a  footer"}]
         (parser/parse "extends-file"))))

(deftest extends-from-parent-dir
  (is (= [{:type  :text
           :value "this is a header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a  footer"}]
         (parser/parse "subfolder/extends-from-parent-dir"))))

(deftest extends-from-sub-dir
  (is (= [{:type  :text
           :value "this is a subfolder header"}
          {:type  :text
           :value "hello world"}
          {:type  :text
           :value "this is a subfolder footer"}]
         (parser/parse "subfolder/extends-from-sub-dir"))))


