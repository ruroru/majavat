(ns jj.majavat.render-test
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [are deftest is testing]]
    [jj.majavat.parser :as parser]
    [jj.majavat.renderer :as renderer :refer [->InputStreamRenderer ->StringRenderer]]
    [jj.majavat.renderer.sanitizer :refer [->Html]]
    [jj.majavat.resolver.fs :as fcr]
    [jj.majavat.resolver.resource :as rcr])
  (:import (java.io InputStream)
           (java.net URI)
           (java.time LocalDate LocalDateTime LocalTime ZoneId ZonedDateTime)
           (java.util UUID)))


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
         (crlf->lf (renderer/render (->StringRenderer {}) template context))) "string assertion")
  (is (= (crlf->lf expected-string)
         (crlf->lf (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer {}) template context)))))
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
    (are [input expected] (= (format "hello %s" expected) (renderer/render (->StringRenderer {:sanitizer (->Html)}) template {:name input}))
                          "&" "&amp;"
                          "<" "&lt;"
                          ">" "&gt;"
                          "\"" "&quot;")))

(deftest if-not-test
  (let [context {:value "some"}]
    (are [rendered-string template-path]

      (= rendered-string
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context)))
         (renderer/render (->StringRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context))
      "hello " "if/if-not"
      "hello universe" "if/if-not-else")))


(deftest render-filters
  (System/setProperty "user.timezone" "UTC")

  (are [expected-value template-path context]
    (let []
      (= expected-value
         (renderer/render (->StringRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context)
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context)))))
    "foo BAR" "filter/upper-case" {:value "bar"}
    "foo " "filter/upper-case" {}
    "foo bar" "filter/lower-case" {:value "BAR"}
    "foo " "filter/lower-case" {}
    "Foo Bar" "filter/capitalize" {:value "BAR"}
    "Foo " "filter/capitalize" {}
    "foo the strive LXXXIV ivy" "filter/upper-roman" {:value "lxxxIv"}
    "foo the strive  ivy" "filter/upper-roman" {}
    "Foo  Bar Baz Qux  Quux Foo-Bar" "filter/title-case" {:value "bar baz qux  quux foo-bar"}
    "foo bar baz qux  quux" "filter/trim" {:value "  bar baz qux  quux  "}
    "foo " "filter/trim" {}
    "foo BAR BAZ QUX  QUUX" "filter/multi-filter" {:value "  bar baz qux  quux  "}
    "foo keyword" "filter/keyword" {:value :keyword}
    "id is 3" "filter/inc" {:id 2}
    "id is " "filter/inc" {}
    "id is 1" "filter/dec" {:id 2}
    "id is " "filter/dec" {}
    "file sizes are: 120.2 KB, 67.8 KB and 5 PB" "filter/file-size" {:file1 123123 :file2 "69420" :file3 "5629499534213120"}
    "file sizes are: ,  and " "filter/file-size" {}
    "foo baz" "filter/default" {}
    "foo bar" "filter/default" {:value "bar"}
    "yyyy is 2022 and yyyy/mm/dd is 2022/01/01" "filter/date" {:value (LocalDate/of 2022, 01, 01)}
    "default: 2022-01-01T01:01, format one is 01/01/2022 01:01" "filter/date-local-date-time" {:value (LocalDateTime/of 2022, 01, 01, 01, 01)}
    "default: 01:01, format one is 01/01" "filter/date-local-time" {:value (LocalTime/of 01, 01)}
    "default: 2022-01-02T03:04Z[UTC], format one is 2022-01-02 03:04 and time in tokyo is 2022-01-02 12:04" "filter/date-zoned-date-time" {:value (ZonedDateTime/of (LocalDateTime/of 2022, 01, 02, 03, 04) (ZoneId/of "UTC"))}
    "value is 550e8400-e29b-41d4-a716-446655440000" "filter/value" {:value (UUID/fromString "550e8400-e29b-41d4-a716-446655440000")}
    "value is http://www.example.com" "filter/value" {:value (.toURL (URI. "http://www.example.com"))}
    "value is /some/path" "filter/value" {:value (URI. "/some/path")}
    "testing [{&quot;key1&quot; &quot;value1&quot;, &quot;value&quot; &quot;b&quot;}]" "filter/where" {:value [{"key1" "value1" "value" "b"} {"key1" "value2" "value" "b"}]}
    ))


(deftest render-let-value
  (testing "parsing as a string"
    (are [expected-value template-path context]
      (= expected-value
         (renderer/render (->StringRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context))
      "testing hello barbaz" "let/let-foo" {}
      "testing hello barbaz" "let/let-bar" {:bar {:qux "bar"}}
      "testing hello barbaz" "let/let-qux" {:bar {:qux "bar"}}))
  (testing "parsing to inpustream"
    (are [expected-value template-path context]
      (= expected-value
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context))))
      "testing hello barbaz" "let/let-foo" {}
      "testing hello barbaz" "let/let-bar" {:bar {:qux "bar"}}
      "testing hello barbaz" "let/let-qux" {:bar {:qux "bar"}})))


(deftest loop-over-sequence
  (are [expected template-path]
    (let [context {:planets ["Mercury" "Venus" "Earth" "Mars" "Jupiter" "Saturn" "Uranus" "Neptune"]}
          render-result (renderer/render (->StringRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context)
          render-is-result (String. (.readAllBytes ^InputStream (renderer/render
                                                                  (->InputStreamRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context)))]

      (and (= expected render-result)
           (= expected render-is-result)
           (= render-result render-is-result)))

    (slurp (io/resource "loop/expected-for-loop")) "loop/for-loop"))

(deftest csrf-token
  (testing "render to string"
    (are [expected-value template-path context]
      (= expected-value
         (renderer/render (->StringRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context))
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"bar\"> " "csrf/csrf" {:csrf-token "bar"}
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"\"> " "csrf/csrf" {}))
  (testing "render to input stream"
    (are [expected-value template-path context]
      (= expected-value
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context))))
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"bar\"> " "csrf/csrf" {:csrf-token "bar"}
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"\"> " "csrf/csrf" {})))

(deftest query-string
  (testing "render to string"
    (are [expected-value template-path context]
      (= expected-value
         (renderer/render (->StringRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context))
      "/some/route" "query-string/query-string" {}
      "/some/route?key=%23%20%3F%20%26" "query-string/query-string" {:foo {:bar {:key "# ? &"}}}
      "/some/route?key=value" "query-string/query-string" {:foo {:bar {"key" "value"}}}
      "/some/route?key=value&key1=value1" "query-string/query-string" {:foo {:bar {:key "value" :key1 "value1"}}}))
  (testing "render to input stream"
    (are [expected-value template-path context]
      (= expected-value
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context))))
      "/some/route" "query-string/query-string" {}
      "/some/route?key=value" "query-string/query-string" {:foo {:bar {:key "value"}}}
      "/some/route?key=value&key1=value1" "query-string/query-string" {:foo {:bar {:key "value" :key1 "value1"}}})))

(deftest now-default
  (testing "render to string"
    (are [timestamp-regex template-path context]
      (is (re-find timestamp-regex (renderer/render (->StringRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context))
          (str "Expected timestamp format in result: " (renderer/render (->StringRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context)))
      #"20\d{2}/\d{2}/\d{2} \d{2}:\d{2}" "now/now" {}
      #"20\d{2}-\d{2}-\d{2}" "now/now-with-format-and-time-zone" {}
      ))
  (testing "render to input stream"
    (are [timestamp-regex template-path context]
      (is (re-find timestamp-regex (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context))))
          (str "Expected timestamp format in result: " (renderer/render (->InputStreamRenderer {:sanitizer (->Html)}) (parser/parse template-path contentResolver) context)))
      #"20\d{2}/\d{2}/\d{2} \d{2}:\d{2}" "now/now" {}
      #"20\d{2}-\d{2}-\d{2}" "now/now-with-format" {}
      #"20\d{2}-\d{2}-\d{2}" "now/now-with-format-and-time-zone" {})))


(deftest for-values
  (are [expected input-context]
    (= (crlf->lf expected)
       (crlf->lf (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer {}) (parser/parse "loop/for-values" contentResolver) input-context))))
       (crlf->lf (renderer/render (->StringRenderer {}) (parser/parse "loop/for-values" contentResolver) input-context)))
    "first true false 2 0\nsecond false true 2 1\n" {:values (list "first" "second")}
    "first true true 1 0\n" {:values (list "first")}
    "" {:values (list)}
    ))
