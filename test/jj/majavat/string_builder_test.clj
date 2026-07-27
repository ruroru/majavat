(ns jj.majavat.string-builder-test
  (:require [clojure.test :refer [deftest is testing]]
            [jj.majavat.string-builder :as sb]))

(deftest empty-builder
  (is (= "" (sb/build (sb/create-string-builder)))))

(deftest append-string
  (let [b (sb/create-string-builder)]
    (sb/append b "hello")
    (sb/append b " world")
    (is (= "hello world" (sb/build b)))))

(deftest append-char
  (let [b (sb/create-string-builder)]
    (sb/append b \a)
    (sb/append b \b)
    (is (= "ab" (sb/build b)))))

(deftest append-returns-builder
  (testing "append returns the builder so calls can be threaded"
    (let [b (sb/create-string-builder)]
      (is (identical? b (sb/append b "x")))
      (-> b (sb/append "y") (sb/append "z"))
      (is (= "xyz" (sb/build b))))))
