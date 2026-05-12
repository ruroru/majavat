(ns jj.majavat.stream-test
  (:require [clojure.test :refer :all])
  (:import
    (java.io ByteArrayOutputStream)
    (java.util ArrayList List)
    (jj.majavat.stream SequentialByteArrayInputStream)))

(deftest read-all-bytes-test
  (are [expected input] (= expected (String. (.readAllBytes (SequentialByteArrayInputStream. (ArrayList. ^List input)))))
                        "" []
                        "a" [(.getBytes "a")]
                        "一二三" [
                                  (.getBytes "一")
                                  (.getBytes "二")
                                  (.getBytes "三")
                                  ]))


(deftest available-test
  (are [expected input] (= expected (.available ^SequentialByteArrayInputStream (SequentialByteArrayInputStream. (ArrayList. ^List input))))
                        0 []
                        1 [(.getBytes "a")]
                        3 [
                           (.getBytes "一")
                           (.getBytes "二")
                           (.getBytes "三")
                           ]))

(deftest read-single-byte-test
  (are [expected input] (= expected (.read (SequentialByteArrayInputStream. (ArrayList. ^List input))))
                        -1 []
                        97 [(.getBytes "a")]
                        228 [(.getBytes "一")]))

(deftest read-byte-array-test
  (let [buf (byte-array 10)]
    (are [expected input] (let [stream (SequentialByteArrayInputStream. (ArrayList. ^List input))
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
    (are [expected input] (let [stream (SequentialByteArrayInputStream. (ArrayList. ^List input))
                                n (.read stream buf 2 5)]
                            (= expected n))
                          -1 []
                          1 [(.getBytes "a")]
                          5 [(.getBytes "一")
                             (.getBytes "二")])))



(deftest read-after-close-test
  (are [input] (let [stream (SequentialByteArrayInputStream. (ArrayList. ^List input))]
                 (.close stream)
                 (= -1 (.read stream)))
               []
               [(.getBytes "a")]
               [(.getBytes "一")]))

(deftest multiple-reads-test
  (are [expected input] (let [stream (SequentialByteArrayInputStream. (ArrayList. ^List input))
                              first (.read stream)
                              second (.read stream)
                              third (.read stream)]
                          (= expected [first second third]))
                        [-1 -1 -1] []
                        [97 -1 -1] [(.getBytes "a")]
                        [228 184 128] [(.getBytes "一")]))

(deftest transfer-to-test
  (are [expected input] (let [stream (SequentialByteArrayInputStream. (ArrayList. ^List input))
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

(deftest skip-test
  (testing "Basic skipping logic"
    (are [expected-skip expected-remaining input n]
      (let [stream (SequentialByteArrayInputStream. (ArrayList. ^List input))
            actual-skip (.skip stream n)
            remaining (String. (.readAllBytes stream))]
        (and (= expected-skip actual-skip)
             (= expected-remaining remaining)))

      0 "abc" [(.getBytes "abc")] 0
      2 "c" [(.getBytes "abc")] 2
      3 "" [(.getBytes "abc")] 5
      3 "def" [(.getBytes "abc") (.getBytes "def")] 3
      4 "ef" [(.getBytes "abc") (.getBytes "def")] 4
      0 "" [] 10))

  (testing "Negative skip"
    (let [stream (SequentialByteArrayInputStream. (ArrayList. [(.getBytes "abc")]))]
      (is (= 0 (.skip stream -1)) "Negative skip should return 0"))))

(deftest available-test
  (testing "Initial available count"
    (are [expected input] (= expected (.available ^SequentialByteArrayInputStream (SequentialByteArrayInputStream. input)))
                          0 (ArrayList.)
                          1 (ArrayList. [(.getBytes "a")])
                          6 (ArrayList. [(.getBytes "one") (.getBytes "two")])
                          ))

  (testing "Available count decreases as we read"
    (let [stream (SequentialByteArrayInputStream. (ArrayList. [(.getBytes "abc") (.getBytes "de")]))]
      (is (= 5 (.available stream)))
      (.read stream)
      (is (= 4 (.available stream)))
      (.read stream)
      (.read stream)
      (is (= 2 (.available stream)))))

  (testing "Available after close"
    (let [stream (SequentialByteArrayInputStream. (ArrayList. [(.getBytes "abc")]))]
      (.close stream)
      (is (= 0 (.available stream))))))