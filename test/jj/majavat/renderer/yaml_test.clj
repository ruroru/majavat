(ns jj.majavat.renderer.yaml-test
  (:require
    [clojure.test :refer [are deftest is]]
    [jj.majavat.protocol.yaml :as protocol]
    [jj.majavat.renderer.yaml :as yaml]))

(def ^:private default (yaml/->DefaultYamlSerializer))

(deftest default-serializer-scalars-test
  (are [input expected] (= expected (protocol/to-yaml default input nil))
                        nil "null"
                        true "true"
                        false "false"
                        1 "1"
                        1.5 "1.5"
                        1/2 "0.5"
                        "hello" "hello"
                        "hello world" "hello world"
                        "1.2.3" "1.2.3"
                        :foo "foo"
                        :ns/foo "ns/foo"
                        {} "{}"
                        [] "[]"))

(deftest default-serializer-quotes-when-plain-is-unsafe-test
  (are [input expected] (= expected (protocol/to-yaml default input nil))
                        "" "\"\""
                        "yes" "\"yes\""
                        "NULL" "\"NULL\""
                        "true" "\"true\""
                        "123" "\"123\""
                        "-1.5e3" "\"-1.5e3\""
                        "key: value" "\"key: value\""
                        "trailing:" "\"trailing:\""
                        "value # not a comment" "\"value # not a comment\""
                        "- dash" "\"- dash\""
                        "  padded" "\"  padded\""
                        "padded  " "\"padded  \""
                        "a\"b\\c\td" "\"a\\\"b\\\\c\\td\""))

(deftest default-serializer-nan-infinity-test
  (are [input expected] (= expected (protocol/to-yaml default input nil))
                        (/ 1.0 0.0) ".inf"
                        (/ -1.0 0.0) "-.inf"
                        (Double/NaN) ".nan"))

(deftest default-serializer-blocks-test
  (are [input expected] (= expected (protocol/to-yaml default input nil))
                        {:a 1} "a: 1"
                        {:a 1 :b 2} "a: 1\nb: 2"
                        {:a nil} "a: null"
                        {:a {:b 1}} "a:\n  b: 1"
                        {:a {:b {:c 1}}} "a:\n  b:\n    c: 1"
                        {:a []} "a: []"
                        {:a {}} "a: {}"
                        [1 2 3] "- 1\n- 2\n- 3"
                        (list "a" "b") "- a\n- b"
                        {:a [1 2]} "a:\n  - 1\n  - 2"
                        [{:a 1} {:b 2}] "- a: 1\n- b: 2"
                        [[1 2] [3]] "-\n  - 1\n  - 2\n-\n  - 3"
                        {:servers [{:name "web" :port 8080}
                                   {:name "db" :port 5432}]}
                        "servers:\n  - name: web\n    port: 8080\n  - name: db\n    port: 5432"))

(deftest default-serializer-literal-block-test
  (are [input expected] (= expected (protocol/to-yaml default input nil))
                        "line1\nline2" "|-\n  line1\n  line2"
                        {:a "line1\nline2"} "a: |-\n  line1\n  line2"
                        {:a "line1\nline2\n"} "a: |\n  line1\n  line2\n"
                        {:a "x\ny\n" :b 1} "a: |\n  x\n  y\n\nb: 1"
                        {:a "line1\n\nline3"} "a: |-\n  line1\n\n  line3"
                        {:a "line1\n  indented"} "a: |-\n  line1\n    indented"
                        {:a "say \"hi\"\nback\\slash"} "a: |-\n  say \"hi\"\n  back\\slash"
                        {:a "x\ny" :b 1} "a: |-\n  x\n  y\nb: 1"
                        {:a {:b "x\ny"}} "a:\n  b: |-\n    x\n    y"
                        ["a\nb"] "- |-\n  a\n  b"
                        [{:a "x\ny"}] "- a: |-\n    x\n    y"))

(deftest default-serializer-quotes-what-a-literal-block-cannot-carry-test
  (are [input expected] (= expected (protocol/to-yaml default input nil))
                        "abc\n" "\"abc\\n\""
                        "a\nb\n\n" "\"a\\nb\\n\\n\""
                        "a \nb" "\"a \\nb\""
                        "a\nb " "\"a\\nb \""
                        "a\r\nb" "\"a\\r\\nb\""
                        " a\nb" "\" a\\nb\""
                        "\n\n" "\"\\n\\n\""
                        {"a\nb" 1} "\"a\\nb\": 1"))

(deftest default-serializer-indent-test
  (are [input indent expected] (= expected (protocol/to-yaml default input {:indent indent}))
                               {:a {:b 1}} 4 "a:\n    b: 1"
                               {:a [1 2]} 4 "a:\n    - 1\n    - 2"
                               {:a [{:b 1 :c 2}]} 4 "a:\n    - b: 1\n      c: 2"
                               {:a "x\ny"} 4 "a: |-\n    x\n    y"
                               {:a {:b 1}} :4 "a:\n    b: 1"
                               {:a {:b 1}} 0 "a:\n  b: 1"
                               {:a {:b 1}} nil "a:\n  b: 1"))

(defrecord UppercaseSerializer []
  protocol/Yaml
  (to-yaml [_ value _opts]
    (str "<<" (clojure.string/upper-case (str value)) ">>")))

(deftest custom-serializer-test
  (is (= "<<HELLO>>" (protocol/to-yaml (->UppercaseSerializer) "hello" nil)))
  (is (= "<<HELLO>>" (protocol/to-yaml (->UppercaseSerializer) "hello" {:indent 2}))))
