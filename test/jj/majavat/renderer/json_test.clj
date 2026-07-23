(ns jj.majavat.renderer.json-test
  (:require
    [clojure.test :refer [are deftest is]]
    [jj.majavat.protocol.json :as protocol]
    [jj.majavat.renderer.json :as json]))

(def ^:private default (json/->DefaultJsonSerializer))

(deftest default-serializer-test
  (are [input expected] (= expected (protocol/to-json default input nil))
                        nil "null"
                        true "true"
                        false "false"
                        1 "1"
                        1.5 "1.5"
                        1/2 "0.5"
                        "hello" "\"hello\""
                        "" "\"\""
                        "a\"b\\c\nd\te" "\"a\\\"b\\\\c\\nd\\te\""
                        :foo "\"foo\""
                        :ns/foo "\"ns/foo\""
                        [1 2 3] "[1,2,3]"
                        [] "[]"
                        (list "a" "b") "[\"a\",\"b\"]"
                        {} "{}"
                        {:a 1} "{\"a\":1}"
                        {"k" "v"} "{\"k\":\"v\"}"
                        [{:a 1} {:b 2}] "[{\"a\":1},{\"b\":2}]"
                        {:a [1 2] :b nil} "{\"a\":[1,2],\"b\":null}"
                        {:nested {:x true}} "{\"nested\":{\"x\":true}}"))

(deftest default-serializer-nan-infinity-test
  (are [input expected] (= expected (protocol/to-json default input nil))
                        (/ 1.0 0.0) "null"
                        (/ -1.0 0.0) "null"
                        (Double/NaN) "null"))

(deftest default-serializer-indent-test
  (are [input indent expected] (= expected (protocol/to-json default input {:indent indent}))
                               {:a 1} 2 "{\n  \"a\": 1\n}"
                               [1 2] 2 "[\n  1,\n  2\n]"
                               {:a {:b 1}} 2 "{\n  \"a\": {\n    \"b\": 1\n  }\n}"
                               {} 2 "{}"
                               [] 2 "[]"
                               {:a 1} :4 "{\n    \"a\": 1\n}"))

(defrecord UppercaseSerializer []
  protocol/Json
  (to-json [_ value _opts]
    (str "<<" (clojure.string/upper-case (str value)) ">>")))

(deftest custom-serializer-test
  (is (= "<<HELLO>>" (protocol/to-json (->UppercaseSerializer) "hello" nil)))
  (is (= "<<HELLO>>" (protocol/to-json (->UppercaseSerializer) "hello" {:indent 2}))))
