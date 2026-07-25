(ns jj.majavat.expand-test
  (:require [clojure.test :refer [are deftest is]]
            [clojure.walk :as walk]
            [jj.majavat.parser :as parser]
            [jj.majavat.renderer.sanitizer :refer [->None]]
            [jj.majavat.renderer.json :refer [->DefaultJsonSerializer]]
            [jj.majavat.protocol.mock-dictionary :refer [create-mock-dictionary]]
            [jj.majavat.resolver.resource :as rcr]))

(def empty-fn-map {})
(def empty-sanitizers-map {})
(def ^:private default-dictionary (create-mock-dictionary))
(def ^:private default-sanitizer (->None))
(def ^:private default-json-serializer (->DefaultJsonSerializer))

(defn- expand-raw [& args]
  (parser/expand-macros
    (apply parser/parse
           (concat args (drop (- (count args) 4)
                              [default-dictionary default-sanitizer default-json-serializer])))))

(defn- expand [& args]
  (walk/postwalk #(if (map? %) (dissoc % :render-fn) %) (apply expand-raw args)))


(deftest csrf-token-expands
  (is (= [{:type :text :value "foo "}
          {:type :text :value "<input type=\"hidden\" name=\"csrf_token\" value=\""}
          {:type :value-node}
          {:type :text :value "\">"}
          {:type :text :value " "}
          {:type :value-node}]
         (expand "csrf/csrf" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))


(deftest query-string-expands-to-value-node
  (is (= [{:type :text :value "/some/route"}
          {:type :value-node}]
         (expand "query-string/query-string" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))


(deftest macro
  (let [expected-ast [{:type :text :value "bar"}
                      {:type :value-node}
                      {:type :text :value "bar"}
                      {:type :value-node}]]
    (is (= expected-ast (expand "macro/macro" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))


(deftest macro-open-paren
  (let [expected-ast [{:type :text :value "bar"}
                      {:type :value-node}
                      {:type :text :value "bar"}
                      {:type :value-node}]]
    (is (= expected-ast (expand "macro/macro-open-paren" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))


(deftest unknown-block-tag-is-error
  (let [result (expand "macro/macro-unknown" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)]
    (is (= "syntax-error" (:type result)))
    (is (= "unknown tag or macro 'bogus' on line 1" (:error-message result)))))


(deftest macro-with-argument
  (let [expected-ast [{:type :text :value "hello "}
                      {:type :value-node}
                      {:type :text :value "!"}
                      {:type :text :value "hello "}
                      {:type :value-node}
                      {:type :text :value "!"}]
        input-file "macro/macro-with-arg"
        result (expand-raw input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)]
    (is (= expected-ast (expand input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))
    (is (= "bob" ((:render-fn (nth result 1)) {:name "bob"})))
    (is (= "alice" ((:render-fn (nth result 4)) {:user {:name "alice"}})))))


(deftest macro-with-literal-argument
  (let [expected-ast [{:type :text :value "hello "}
                      {:type :text :value "world"}
                      {:type :text :value "!"}]]
    (is (= expected-ast (expand "macro/macro-with-literal-arg" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))


(deftest macro-with-two-params
  (let [expected-ast [{:type :text :value "hello"}
                      {:type :text :value "foo"}
                      {:type :text :value "bar"}]]
    (is (= expected-ast (expand "macro/two-param-macro" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))


(deftest macro-called-before-definition
  (is (= [{:type :text :value "hi "}
          {:type :text :value "bob"}
          {:type :text :value "!"}]
         (expand "macro/forward-reference" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))))


(deftest macro-called-with-not-enough-args
  (let [expected-ast {:error-message "error on line 1"
                      :line          "1"
                      :type          "syntax-error"}]
    (is (= expected-ast (expand "macro/two-param-macro-not-enough-args" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)))))
