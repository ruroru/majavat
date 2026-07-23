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

(deftest is-empty-returns-true
  (are [input] (= true (tests/is-empty? input))
               nil
               ""
               []
               {}
               #{}
               '()))

(deftest is-empty-returns-false
  (are [input] (= false (tests/is-empty? input))
               "string"
               :keyword
               true
               false
               0
               1
               [1]
               {:a 1}
               #{:a}))



(deftest is-ge-returns-true
  (are [v arg] (= true (tests/is-ge? v arg))
               10 5
               5  5
               0  0
               -1 -5))

(deftest is-ge-returns-false
  (are [v arg] (= false (boolean (tests/is-ge? v arg)))
               3        5
               -5       -1))

(deftest is-gt-returns-true
  (are [v arg] (= true (tests/is-gt? v arg))
               10 5
               1  0
               -1 -5))

(deftest is-gt-returns-false
  (are [v arg] (= false (boolean (tests/is-gt? v arg)))
               5        5
               3        5
               -5       -1))

(deftest is-le-returns-true
  (are [v arg] (= true (tests/is-le? v arg))
               5  10
               5  5
               0  0
               -5 -1))

(deftest is-le-returns-false
  (are [v arg] (= false (boolean (tests/is-le? v arg)))
               10       5
               -1       -5))

(deftest is-lt-returns-true
  (are [v arg] (= true (tests/is-lt? v arg))
               5  10
               0  1
               -5 -1))

(deftest is-lt-returns-false
  (are [v arg] (= false (boolean (tests/is-lt? v arg)))
               5        5
               10       5
               -1       -5
               ))