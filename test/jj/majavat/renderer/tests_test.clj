(ns jj.majavat.renderer.tests-test
  (:require [clojure.test :refer :all]
            [jj.majavat.renderer.tests :as tests]))

(deftest default-returns-true
  (are [input] (= true (tests/default-test input))
               1
               true))

(deftest default-returns-false
  (are [input] (= false (tests/default-test input))
               nil
               false))

(deftest is-even-returns-true
  (are [input] (= true (tests/is-even? input))
               2
               4))

(deftest is-even-returns-false
  (are [input] (= false (tests/is-even? input))
               nil
               "string"
               :keyword
               true
               false
               1
               3))

(deftest is-odd-returns-true
  (are [input] (= true (tests/is-odd? input))
               1
               3))

(deftest is-odd-returns-false
  (are [input] (= false (tests/is-odd? input))
               nil
               "string"
               :keyword
               true
               false
               2
               4))