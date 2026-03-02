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
