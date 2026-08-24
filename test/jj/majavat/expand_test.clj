(ns jj.majavat.expand-test
  (:require [clojure.test :refer [are deftest is]]
            [clojure.walk :as walk]
            [jj.majavat.parser :as parser]
            [jj.majavat.renderer.sanitizer :refer [->None]]
            [jj.majavat.renderer.json :refer [->DefaultJsonSerializer]]
            [jj.majavat.renderer.yaml :refer [->DefaultYamlSerializer]]
            [jj.majavat.protocol.mock-dictionary :refer [create-mock-dictionary]]
            [jj.majavat.resolver.resource :as rcr]))

(def empty-fn-map {})
(def empty-sanitizers-map {})
(def ^:private default-dictionary (create-mock-dictionary))
(def ^:private default-sanitizer (->None))
(def ^:private default-json-serializer (->DefaultJsonSerializer))
(def ^:private default-yaml-serializer (->DefaultYamlSerializer))

(defn- expand-raw [& args]
  (parser/expand-macros
    (apply parser/parse
           (concat args (drop (- (count args) 4)
                              [default-dictionary default-sanitizer default-json-serializer default-yaml-serializer])))))

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


(deftest now-expands-to-value-node
  (are [input-file] (= [{:type :text :value "current time is "}
                        {:type :value-node}]
                       (expand input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    "now/now"
    "now/now-with-format"))


(deftest now-value-node-evaluates-itself-at-render-time
  (let [result    (expand-raw "now/now-with-format-and-time-zone" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)
        render-fn (:render-fn (nth result 1))]
    (is (re-matches #"\d{4}-\d{2}-\d{2}" (render-fn {})))
    (is (nil? (render-fn {} ::raw)))))


(deftest now-with-too-many-args-is-error
  (let [result (expand "now/now-too-many-args" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)]
    (is (= "syntax-error" (:type result)))
    (is (= "1" (:line result)))))


(deftest trans-expands-to-value-node
  (are [input-file] (= [{:type :value-node}]
                       (expand input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map))
    "trans/trans"
    "trans/trans-quoted-key"))


(deftest trans-value-node-translates-key-for-context-locale
  (are [input-file] (let [result    (expand-raw input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)
                          render-fn (:render-fn (first result))]
                      (and (= "hello" (render-fn {:locale "en"}))
                           (= "hei" (render-fn {:locale "fi"}))))
    "trans/trans"
    "trans/trans-quoted-key"))


(deftest trans-without-translation-renders-nothing
  (are [input-file dictionary] (let [result    (expand-raw input-file (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map dictionary)
                                     render-fn (:render-fn (first result))]
                                 (= "" (render-fn {:locale "en"})))
    "trans/trans-unknown-key" default-dictionary
    "trans/trans" nil))


(deftest trans-without-key-is-error
  (let [result (expand "trans/trans-no-args" (rcr/->ResourceResolver) empty-fn-map empty-sanitizers-map)]
    (is (= "syntax-error" (:type result)))
    (is (= "1" (:line result)))))


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
