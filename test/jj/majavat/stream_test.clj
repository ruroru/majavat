(ns jj.majavat.stream-test
  (:require [clojure.test :refer :all])
  (:import
    (java.io ByteArrayOutputStream)
    (jj.majavat.stream SequentialByteArrayInputStream)))

(deftest read-all-bytes-test
  (are [expected input] (= expected (String. (.readAllBytes (SequentialByteArrayInputStream. input))))
                        "" []
                        "a" [(.getBytes "a")]
                        "一二三" [
                                  (.getBytes "一")
                                  (.getBytes "二")
                                  (.getBytes "三")
                                  ]))


(deftest available-test
  (are [expected input] (= expected (.available ^SequentialByteArrayInputStream (SequentialByteArrayInputStream. input)))
                        0 []
                        1 [(.getBytes "a")]
                        3 [
                           (.getBytes "一")
                           (.getBytes "二")
                           (.getBytes "三")
                           ]))

(deftest read-single-byte-test
  (are [expected input] (= expected (.read (SequentialByteArrayInputStream. input)))
                        -1 []
                        97 [(.getBytes "a")]
                        228 [(.getBytes "一")]))

(deftest read-byte-array-test
  (let [buf (byte-array 10)]
    (are [expected input] (let [stream (SequentialByteArrayInputStream. input)
                                n (.read stream buf)]
                            (= expected n))
                          -1 []
                          1 [(.getBytes "a")]
                          9 [(.getBytes "一")
                             (.getBytes "二")
                             (.getBytes "三")]
                          10 [(.getBytes "abcdefghij")]
                          10 [(.getBytes "abcdefghijklmno")]
                          10 [(.getBytes "hello")
                              (.getBytes "world")]
                          7 [(.getBytes "test")
                             (.getBytes "一")]               ;
                          10 [(byte-array [1 2 3 4 5])
                              (byte-array [6 7 8 9 10])]
                          5 [(byte-array [0 0 0 0 0])])))

(deftest read-with-offset-test
  (let [buf (byte-array 10)]
    (are [expected input] (let [stream (SequentialByteArrayInputStream. input)
                                n (.read stream buf 2 5)]
                            (= expected n))
                          -1 []
                          1 [(.getBytes "a")]
                          5 [(.getBytes "一")
                             (.getBytes "二")])))



(deftest read-after-close-test
  (are [input] (let [stream (SequentialByteArrayInputStream. input)]
                 (.close stream)
                 (= -1 (.read stream)))
               []
               [(.getBytes "a")]
               [(.getBytes "一")]))

(deftest multiple-reads-test
  (are [expected input] (let [stream (SequentialByteArrayInputStream. input)
                              first (.read stream)
                              second (.read stream)
                              third (.read stream)]
                          (= expected [first second third]))
                        [-1 -1 -1] []
                        [97 -1 -1] [(.getBytes "a")]
                        [228 184 128] [(.getBytes "一")]))

(deftest transfer-to-test
  (are [expected input] (let [stream (SequentialByteArrayInputStream. input)
                              out (ByteArrayOutputStream.)]
                          (.transferTo stream out)
                          (= expected (String. (.toByteArray out))))
                        "" []
                        "a" [(.getBytes "a")]
                        "abc" [(.getBytes "a")
                               (.getBytes "b")
                               (.getBytes "c")]
                        "一二三" [(.getBytes "一")
                                  (.getBytes "二")
                                  (.getBytes "三")]
                        "hello" [(.getBytes "hello")]
                        "helloworld" [(.getBytes "hello")
                                      (.getBytes "world")]
                        "混合text" [(.getBytes "混")
                                    (.getBytes "合")
                                    (.getBytes "text")]
                        "abcdefghijklmnopqrstuvwxyz" [(.getBytes "abcdefghijklmnopqrstuvwxyz")]
                        "line1\nline2\nline3" [(.getBytes "line1\n")
                                               (.getBytes "line2\n")
                                               (.getBytes "line3")]
                        "emoji😀test" [(.getBytes "emoji")
                                      (.getBytes "😀")
                                      (.getBytes "test")]
                        "spaces   and\ttabs" [(.getBytes "spaces   ")
                                              (.getBytes "and\ttabs")]
                        "special!@#$%^&*()" [(.getBytes "special")
                                             (.getBytes "!@#$%^&*()")]
                        "日本語English한글" [(.getBytes "日本語")
                                           (.getBytes "English")
                                           (.getBytes "한글")]
                        "a" [(.getBytes "")
                             (.getBytes "a")
                             (.getBytes "")]
                        "ab" [(.getBytes "a")
                              (.getBytes "")
                              (.getBytes "b")]
                        "many small parts" [(.getBytes "m")
                                            (.getBytes "a")
                                            (.getBytes "n")
                                            (.getBytes "y")
                                            (.getBytes " ")
                                            (.getBytes "s")
                                            (.getBytes "m")
                                            (.getBytes "a")
                                            (.getBytes "l")
                                            (.getBytes "l")
                                            (.getBytes " ")
                                            (.getBytes "p")
                                            (.getBytes "a")
                                            (.getBytes "r")
                                            (.getBytes "t")
                                            (.getBytes "s")]))
