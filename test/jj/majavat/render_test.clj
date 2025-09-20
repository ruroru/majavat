(ns jj.majavat.render-test
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [are deftest is testing]]
    [jj.majavat.parser :as parser]
    [jj.majavat.renderer :as renderer]
    [jj.majavat.renderer.escape.html :refer [->Html]]
    [jj.majavat.resolver.fs :as fcr]
    [jj.majavat.resolver.resource :as rcr])
  (:import (java.io InputStream)
           (java.time LocalDate)))


(defn- crlf->lf [s]
  (str/replace s "\r\n" "\n"))

(def contentResolver (rcr/->ResourceResolver))
(deftest prerender-test
  (are [expected instructions context]
    (= expected (renderer/pre-render instructions context))

    [{:type :text :value "hello world"}
     {:type :value-node :value :not-existing}]
    [{:type :text :value "hello "}
     {:type :value-node :value :name}
     {:type :value-node :value :not-existing}]
    {:name "world"}

    [{:type :text :value "hello "}
     {:type :value-node :value :name}
     {:type :value-node :value :age}]
    [{:type :text :value "hello "}
     {:type :value-node :value :name}
     {:type :value-node :value :age}]
    {}

    [{:type :value-node :value :missing}
     {:type :value-node :value :also-missing}]
    [{:type :value-node :value :name}
     {:type :value-node :value :missing}
     {:type :value-node :value :age}
     {:type :value-node :value :also-missing}]
    {:name "Alice" :age 30}

    [{:type :text :value "Hello Alice, You are 30"}]
    [{:type :text :value "Hello "}
     {:type :value-node :value :name}
     {:type :text :value ", You are "}
     {:type :value-node :value :age}]
    {:name "Alice" :age 30}

    [{:type :text :value "Department: Engineering"}
     {:type :for :identifier :employee :source [:dept :employees]}
     {:type :value-node :value :missing-budget}
     {:type :if :condition [:dept :active]}]
    [{:type :text :value "Department: "}
     {:type :value-node :value :name}
     {:type :for :identifier :employee :source [:dept :employees]}
     {:type :value-node :value :missing-budget}
     {:type :if :condition [:dept :active]}]
    {:name "Engineering" :budget "$500K"}

    [{:type :text :value "🎉 Hello 世界!"}
     {:type :value-node :value :missing}]
    [{:type :text :value "🎉 Hello "}
     {:type :value-node :value :greeting}
     {:type :text :value "!"}
     {:type :value-node :value :missing}]
    {:greeting "世界"}

    [{:type :text :value "User: admin"}
     {:type :value-node :value :role}
     {:type :value-node :value :missing}]
    [{:type :text :value "User: "}
     {:type :value-node :value :username}
     {:type :value-node :value :role}
     {:type :value-node :value :missing}]
    {:username "admin"
     :profile  {:role "administrator"}}

    []
    []
    {:name "test"}

    [{:type :text :value "Hello "}
     {:type :value-node :value :name}
     {:type :value-node :value :missing}]
    [{:type :text :value "Hello "}
     {:type :value-node :value :name}
     {:type :value-node :value :missing}]
    {"name" "Alice"}))

(defn assert-render [template context expected-string]
  (is (= (crlf->lf expected-string)
         (crlf->lf (renderer/render template context true))) "string assertion")
  (is (= (crlf->lf expected-string)
         (crlf->lf (String. (.readAllBytes ^InputStream (renderer/render-is template context true)))))
      "input stream assertion"))

(deftest advanced-test
  (let [context {:company {:departments [{:name      "Engineering"
                                          :budget    "$500K"
                                          :employees [{:name       "Alice Johnson"
                                                       :title      "Senior Developer"
                                                       :is_manager true}
                                                      {:name       "Bob Smith"
                                                       :title      "Junior Developer"
                                                       :is_manager false}]}
                                         {:name      "Marketing"
                                          :budget    "$300K"
                                          :employees [{:name       "Carol Davis"
                                                       :title      "Marketing Manager"
                                                       :is_manager true}]}]}}
        template [{:type       :for
                   :identifier :department
                   :source     [:company :departments]
                   :body       [{:type :text :value "Department: "}
                                {:type :value-node :value [:department :name]}
                                {:type :text :value " (Budget: "}
                                {:type :value-node :value [:department :budget]}
                                {:type :text :value ")\n"}
                                {:type       :for
                                 :identifier :employee
                                 :source     [:department :employees]
                                 :body       [{:type       :if
                                               :condition  [:employee :is_manager]
                                               :when-true  [{:type :text :value "👔 MANAGER: "}
                                                            {:type :value-node :value [:employee :name]}
                                                            {:type :text :value " - "}
                                                            {:type :value-node :value [:employee :title]}
                                                            {:type :text :value "\n"}
                                                            {:type       :for
                                                             :identifier :report
                                                             :source     [:employee :direct_reports]
                                                             :body       [{:type :text :value "    └─ "}
                                                                          {:type :value-node :value [:report :name]}
                                                                          {:type :text :value " ("}
                                                                          {:type :value-node :value [:report :role]}
                                                                          {:type :text :value ")\n"}]}]
                                               :when-false [{:type :text :value "👤 "}
                                                            {:type :value-node :value [:employee :name]}
                                                            {:type :text :value " - "}
                                                            {:type :value-node :value [:employee :title]}
                                                            {:type :text :value "\n"}]}]}
                                {:type :text :value "\n"}]}]
        expected-string "Department: Engineering (Budget: $500K)
👔 MANAGER: Alice Johnson - Senior Developer
👤 Bob Smith - Junior Developer

Department: Marketing (Budget: $300K)
👔 MANAGER: Carol Davis - Marketing Manager

"]
    (assert-render template context expected-string)))




(deftest test-inheritance
  (let [expected-string "hello jj from parent header

\"testing your email is: some@mail.com\"
foobarbaz
this is a  footer"
        template (parser/parse "inheritance-test" contentResolver)
        context {:user {:name  "jj"
                        :email "some@mail.com"}}]
    (assert-render template context expected-string)))


(deftest test-not-existing-file
  (let [expected-string (slurp (io/resource "render-template-not-found.html"))
        template (parser/parse "not-existing-file" contentResolver)
        context {}]
    (assert-render template context expected-string)))

(deftest include-not-existing
  (let [expected-string (slurp (io/resource "include/not-existing.html"))
        template (parser/parse "includes-not-existing-test" contentResolver)
        context {}]
    (assert-render template context expected-string)))


(deftest extends-not-existing-file
  (let [expected-string (slurp (io/resource "extends/not-existing-extends-error.html"))
        template (parser/parse "extends-not-existing-test" contentResolver)
        context {}]
    (assert-render template context expected-string)))


(deftest use-file-system-resolver
  (let [expected-string "hello jj from parent header

\"testing your email is: some@mail.com\"
foobarbaz
this is a  footer"
        template (parser/parse "./test/resources/inheritance-test" (fcr/->FsResolver))
        context {:user {:name  "jj"
                        :email "some@mail.com"}}]
    (assert-render template context expected-string)))


(deftest render-alternative-value-when-condition-is-nil
  (let [expected-string "not-posts"
        template (parser/parse "else-conditional-test" contentResolver)
        context {:some {:condition nil}}]
    (assert-render template context expected-string)))

(deftest render-value-when-condition-has-some-value
  (let [expected-string "posts"
        template (parser/parse "conditional-test" contentResolver)
        context {:has {:posts 123}}]
    (assert-render template context expected-string)))

(deftest render-alternative-value-when-condition-is-false
  (let [expected-string ""
        template (parser/parse "conditional-test" contentResolver)
        context {:has {:posts false}}]
    (assert-render template context expected-string)))

(deftest render-value-when-condition-is-true
  (let [expected-string "posts"
        template (parser/parse "conditional-test" contentResolver)
        context {:has {:posts true}}]
    (assert-render template context expected-string)))

(deftest escape-test
  (let [template (parser/parse "insert-value.html" contentResolver)]
    (are [input expected] (= (format "hello %s" expected) (renderer/render template
                                                                           {:name input}
                                                                           {:sanitizer (->Html)}))
                          "&" "&amp;"
                          "<" "&lt;"
                          ">" "&gt;"
                          "\"" "&quot;")))

(deftest escape-set-to-false
  (let [template (parser/parse "insert-value.html" contentResolver)]
    (are [input] (= (format "hello %s" input) (renderer/render template {:name input}
                                                               false))
                 "&"
                 "<"
                 ">"
                 "\"")))

(deftest if-not-test
  (are [rendered-string template-path]
    (= rendered-string
       (renderer/render (parser/parse template-path contentResolver) {:value "some"} nil))
    "hello " "if/if-not"
    "hello universe" "if/if-not-else"))


(deftest render-filters
  (are [expected-value template-path context]
    (= expected-value
       (renderer/render (parser/parse template-path contentResolver) context nil)
       (String. (.readAllBytes ^InputStream (renderer/render-is (parser/parse template-path contentResolver) context nil))))
    "foo BAR" "filter/upper-case" {:value "bar"}
    "foo bar" "filter/lower-case" {:value "BAR"}
    "Foo Bar" "filter/capitalize" {:value "BAR"}
    "foo the strive LXXXIV ivy" "filter/upper-roman" {:value "lxxxIv"}
    "Foo  Bar Baz Qux  Quux Foo-Bar" "filter/title-case" {:value "bar baz qux  quux foo-bar"}
    "foo bar baz qux  quux" "filter/trim" {:value "  bar baz qux  quux  "}
    "foo BAR BAZ QUX  QUUX" "filter/multi-filter" {:value "  bar baz qux  quux  "}
    "foo keyword" "filter/keyword" {:value :keyword}
    "id is 3" "filter/inc" {:id 2}
    "id is 1" "filter/dec" {:id 2}
    "file sizes are: 120.2 KB, 67.8 KB and 5 PB" "filter/file-size" {:file1 123123 :file2 "69420" :file3 "5629499534213120"}
    "foo baz" "filter/default" {}
    "yyyy is 2022 and yyyy/mm/dd is 2022/01/01" "filter/date" {:value (LocalDate/of 2022, 01, 01)}
    ))


(deftest render-let-value
  (testing "parsing as a string"
    (are [expected-value template-path context]
      (= expected-value
         (renderer/render (parser/parse template-path contentResolver) context nil))
      "testing hello barbaz" "let/let-foo" {}
      "testing hello barbaz" "let/let-bar" {:bar {:qux "bar"}}
      "testing hello barbaz" "let/let-qux" {:bar {:qux "bar"}}))
  (testing "parsing to inpustream"
    (are [expected-value template-path context]
      (= expected-value
         (String. (.readAllBytes ^InputStream (renderer/render-is (parser/parse template-path contentResolver) context nil))))
      "testing hello barbaz" "let/let-foo" {}
      "testing hello barbaz" "let/let-bar" {:bar {:qux "bar"}}
      "testing hello barbaz" "let/let-qux" {:bar {:qux "bar"}})))


(deftest loop
  (are [expected template-path]
    (let [context {:planets ["Mercury" "Venus" "Earth" "Mars" "Jupiter" "Saturn" "Uranus" "Neptune"]}
          parsed-template (parser/parse template-path contentResolver)
          render-result (renderer/render parsed-template context nil)
          render-is-result (String. (.readAllBytes ^InputStream (renderer/render-is parsed-template context nil)))]

      (and (= expected render-result)
           (= expected render-is-result)
           (= render-result render-is-result)))

    (slurp (io/resource "loop/expected-for-loop")) "loop/for-loop"))

(deftest csrf-token
  (testing "render to string"
    (are [expected-value template-path context]
      (= expected-value
         (renderer/render (parser/parse template-path contentResolver) context nil))
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"bar\"> " "csrf/csrf" {:csrf-token "bar"}
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"\"> " "csrf/csrf" {}))
  (testing "render to input stream"
    (are [expected-value template-path context]
      (= expected-value
         (String. (.readAllBytes ^InputStream (renderer/render-is (parser/parse template-path contentResolver) context nil))))
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"bar\"> " "csrf/csrf" {:csrf-token "bar"}
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"\"> " "csrf/csrf" {})))

(deftest query-string
  (testing "render to string"
    (are [expected-value template-path context]
      (= expected-value
         (renderer/render (parser/parse template-path contentResolver) context nil))
      "/some/route" "query-string/query-string" {}
      "/some/route?key=value" "query-string/query-string" {:foo {:bar {:key "value"}}}
      "/some/route?key=value" "query-string/query-string" {:foo {:bar {"key" "value"}}}
      "/some/route?key=value&key1=value1" "query-string/query-string" {:foo {:bar {:key "value" :key1 "value1"}}}))
  (testing "render to input stream"
    (are [expected-value template-path context]
      (= expected-value
         (String. (.readAllBytes ^InputStream (renderer/render-is (parser/parse template-path contentResolver) context nil))))
      "/some/route" "query-string/query-string" {}
      "/some/route?key=value" "query-string/query-string" {:foo {:bar {:key "value"}}}
      "/some/route?key=value&key1=value1" "query-string/query-string" {:foo {:bar {:key "value" :key1 "value1"}}})))




