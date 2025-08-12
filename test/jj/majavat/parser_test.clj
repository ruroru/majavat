(ns jj.majavat.parser-test
  (:require [clojure.test :refer [deftest is]]
            [jj.majavat.parser :as parser]
            [jj.majavat.resource-content-resolver :as rcr]))

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


