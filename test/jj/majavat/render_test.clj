(ns jj.majavat.render-test
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [are deftest is testing]]
    [jj.majavat.parser :as parser]
    [jj.majavat.renderer :as renderer :refer [->InputStreamRenderer ->PartialRenderer ->StringRenderer]]
    [jj.majavat.renderer.sanitizer :refer [->Html]]
    [jj.majavat.resolver.fs :as fcr]
    [jj.majavat.resolver.resource :as rcr])
  (:import (java.io InputStream)
           (java.net URI)
           (java.time LocalDate LocalDateTime LocalTime ZoneId ZonedDateTime)
           (java.util UUID)))


(defn- crlf->lf [s]
  (str/replace s "\r\n" "\n"))

(def empty-fn-map {})


(def contentResolver (rcr/->ResourceResolver))
(defrecord Container [value])
(defrecord Container1 [value1])
(defrecord Container2 [value2])
(defrecord Container3 [value3])
(defrecord Container4 [value4])
(defrecord Container5 [value5])
(defrecord Container6 [value6])

(defn assert-render [template context expected-string]
  (is (= (crlf->lf expected-string)
         (crlf->lf (renderer/render (->StringRenderer)
                                    template
                                    context
                                    (->Html)))) "string assertion")
  (is (= (crlf->lf expected-string)
         (crlf->lf (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer)
                                                                         template
                                                                         context
                                                                         nil)))))
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
        template (parser/parse "inheritance-test" contentResolver empty-fn-map)
        context {:user {:name  "jj"
                        :email "some@mail.com"}}]
    (assert-render template context expected-string)))


(deftest test-not-existing-file
  (let [expected-string (slurp (io/resource "render-template-not-found.html"))
        template (parser/parse "not-existing-file" contentResolver empty-fn-map)
        context {}]
    (assert-render template context expected-string)))

(deftest include-not-existing
  (let [expected-string (slurp (io/resource "include/not-existing.html"))
        template (parser/parse "includes-not-existing-test" contentResolver empty-fn-map)
        context {}]
    (assert-render template context expected-string)))


(deftest extends-not-existing-file
  (let [expected-string (slurp (io/resource "extends/not-existing-extends-error.html"))
        template (parser/parse "extends-not-existing-test" contentResolver empty-fn-map)
        context {}]
    (assert-render template context expected-string)))


(deftest use-file-system-resolver
  (let [expected-string "hello jj from parent header

\"testing your email is: some@mail.com\"
foobarbaz
this is a  footer"
        template (parser/parse "./test/resources/inheritance-test" (fcr/->FsResolver) empty-fn-map)
        context {:user {:name  "jj"
                        :email "some@mail.com"}}]
    (assert-render template context expected-string)))


(deftest render-alternative-value-when-condition-is-nil
  (let [expected-string "not-posts"
        template (parser/parse "else-conditional-test" contentResolver empty-fn-map)
        context {:some {:condition nil}}]
    (assert-render template context expected-string)))

(deftest render-value-when-condition-has-some-value
  (let [expected-string "posts"
        template (parser/parse "conditional-test" contentResolver empty-fn-map)
        context {:has {:posts 123}}]
    (assert-render template context expected-string)))

(deftest render-alternative-value-when-condition-is-false
  (let [expected-string ""
        template (parser/parse "conditional-test" contentResolver empty-fn-map)
        context {:has {:posts false}}]
    (assert-render template context expected-string)))

(deftest render-value-when-condition-is-true
  (let [expected-string "posts"
        template (parser/parse "conditional-test" contentResolver empty-fn-map)
        context {:has {:posts true}}]
    (assert-render template context expected-string)))

(deftest escape-test
  (let [template (parser/parse "insert-value.html" contentResolver empty-fn-map)]
    (are [input expected] (= (format "hello %s" expected) (renderer/render (->StringRenderer)
                                                                           template
                                                                           {:name input}
                                                                           (->Html)))
                          "&" "&amp;"
                          "<" "&lt;"
                          ">" "&gt;"
                          "\"" "&quot;")))

(deftest if-not-test
  (let [context {:value "some"}]
    (are [rendered-string template-path]

      (= rendered-string
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer)
                                                               (parser/parse template-path contentResolver empty-fn-map)
                                                               context
                                                               (->Html))))
         (renderer/render (->StringRenderer)
                          (parser/parse template-path contentResolver empty-fn-map)
                          context
                          (->Html)))
      "hello " "if/if-not"
      "hello universe" "if/if-not-else")))


(deftest render-filters
  (System/setProperty "user.timezone" "UTC")

  (are [expected-value template-path context]
    (= expected-value
       (renderer/render (->StringRenderer)
                        (parser/parse template-path contentResolver empty-fn-map)
                        context
                        (->Html))
       (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer)
                                                             (parser/parse template-path contentResolver empty-fn-map)
                                                             context
                                                             (->Html)))))
    "foo BAR1 BAZｲ" "filter/upper-case" {:value "BAR1 BaZｲ"}
    "foo BAR2 BAZｲ" "filter/upper-case" {:value "bar2 BaZｲ"}
    "foo BAR3 BAZｲ" "filter/upper-case" {:value "Bar3 BaZｲ"}
    "foo " "filter/upper-case" {:value ""}
    "foo " "filter/upper-case" {}
    "foo bar1 bazｲ" "filter/lower-case" {:value "BAR1 BaZｲ"}
    "foo bar2 bazｲ" "filter/lower-case" {:value "bar2 BaZｲ"}
    "foo bar3 bazｲ" "filter/lower-case" {:value "Bar3 BaZｲ"}
    "foo " "filter/lower-case" {:value ""}
    "foo " "filter/lower-case" {}
    "Foo Bar1 bazｲ" "filter/capitalize" {:value "BAR1 BaZｲ"}
    "Foo Bar2 bazｲ" "filter/capitalize" {:value "bar2 BaZｲ"}
    "Foo Bar3 bazｲ" "filter/capitalize" {:value "Bar3 BaZｲ"}
    "Foo " "filter/capitalize" {:value ""}
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
    "foo 2.2" "filter/abs" {:value -2.2}
    "foo 3.0" "filter/ceil" {:value 2.2}
    "foo 2.0" "filter/floor" {:value 2.2}
    "foo 2" "filter/round" {:value 2.2}
    "foobarbaz" "filter/append" {:value "bar"}
    "foobazbar" "filter/prepend" {:value "bar"}
    "foobar-baz-quaz" "filter/slugify" {:value "bar baz-Quaz"}
    ))

(deftest render-custom-filter
  (let [filter-map {:quote (fn [value args]
                             (format "\"%s\" - %s" value (first args)))}]
    (are [expected-value template-path context]
      (= expected-value
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer)
                                                               (parser/parse template-path contentResolver filter-map)
                                                               context
                                                               nil)))
         (renderer/render (->StringRenderer)
                          (parser/parse template-path contentResolver filter-map)
                          context
                          nil))
      "\"Foo Bar Baz\" - Sun Tzu" "custom-filter/quote" {:value "Foo Bar Baz"})))


(deftest unsupported-filter
  (are [expected-file-path template-path context]
    (= (crlf->lf (slurp (io/resource expected-file-path)))
       (crlf->lf (renderer/render (->StringRenderer)
                                  (parser/parse template-path contentResolver empty-fn-map)
                                  context
                                  (->Html)))
       (crlf->lf (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer)
                                                                       (parser/parse template-path contentResolver empty-fn-map)
                                                                       context
                                                                       (->Html))))))
    "filter/not-supported-filter-expected" "filter/not-supported-filter" {:value "bar"}))

(deftest render-let-value
  (testing "parsing as a string"
    (are [expected-value template-path context]
      (= expected-value
         (renderer/render (->StringRenderer)
                          (parser/parse template-path contentResolver empty-fn-map)
                          context
                          (->Html)))
      "testing hello barbaz" "let/let-foo" {}
      "testing hello barbaz" "let/let-bar" {:bar {:qux "bar"}}
      "testing hello barbaz" "let/let-qux" {:bar {:qux "bar"}}))
  (testing "parsing to inpustream"
    (are [expected-value template-path context]
      (= expected-value
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer)
                                                               (parser/parse template-path contentResolver empty-fn-map)
                                                               context
                                                               (->Html)))))
      "testing hello barbaz" "let/let-foo" {}
      "testing hello barbaz" "let/let-bar" {:bar {:qux "bar"}}
      "testing hello barbaz" "let/let-qux" {:bar {:qux "bar"}})))


(deftest loop-over-sequence
  (are [expected template-path]
    (let [context {:planets ["Mercury" "Venus" "Earth" "Mars" "Jupiter" "Saturn" "Uranus" "Neptune"]}
          render-result (renderer/render (->StringRenderer)
                                         (parser/parse template-path contentResolver empty-fn-map)
                                         context
                                         (->Html))
          render-is-result (String. (.readAllBytes ^InputStream (renderer/render
                                                                  (->InputStreamRenderer)
                                                                  (parser/parse template-path contentResolver empty-fn-map) context
                                                                  (->Html))))]

      (and (= expected render-result)
           (= expected render-is-result)
           (= render-result render-is-result)))

    (slurp (io/resource "loop/expected-for-loop")) "loop/for-loop"))

(deftest each
  (let [context {:planets ["Mercury" "Venus" "Earth" "Mars" "Jupiter" "Saturn" "Uranus" "Neptune"]}
        string-renderer (renderer/render (->StringRenderer)
                                         (parser/parse "each/each-planet" contentResolver empty-fn-map)
                                         context
                                         (->Html))
        is-renderer (renderer/render
                      (->InputStreamRenderer)
                      (parser/parse "each/each-planet" contentResolver empty-fn-map)
                      context
                      (->Html))]

    (are [expected input]
      (= expected input)

      (slurp (io/resource "each/each-planet-expected")) string-renderer
      (slurp (io/resource "each/each-planet-expected")) (String. (.readAllBytes ^InputStream is-renderer)))))

(deftest csrf-token
  (testing "render to string"
    (are [expected-value template-path context]
      (= expected-value
         (renderer/render (->StringRenderer)
                          (parser/parse template-path contentResolver empty-fn-map)
                          context
                          (->Html)))
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"bar\"> " "csrf/csrf" {:csrf-token "bar"}
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"\"> " "csrf/csrf" {}))
  (testing "render to input stream"
    (are [expected-value template-path context]
      (= expected-value
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer)
                                                               (parser/parse template-path contentResolver empty-fn-map)
                                                               context
                                                               (->Html)))))
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"bar\"> " "csrf/csrf" {:csrf-token "bar"}
      "foo <input type=\"hidden\" name=\"csrf_token\" value=\"\"> " "csrf/csrf" {})))

(deftest query-string
  (testing "render to string"
    (are [expected-value template-path context]
      (= expected-value
         (renderer/render (->StringRenderer)
                          (parser/parse template-path contentResolver empty-fn-map)
                          context
                          (->Html)))
      "/some/route" "query-string/query-string" {}
      "/some/route?key=%23%20%3F%20%26" "query-string/query-string" {:foo {:bar {:key "# ? &"}}}
      "/some/route?key=value" "query-string/query-string" {:foo {:bar {"key" "value"}}}
      "/some/route?key=value&key1=value1" "query-string/query-string" {:foo {:bar {:key "value" :key1 "value1"}}}))
  (testing "render to input stream"
    (are [expected-value template-path context]
      (= expected-value
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer) (parser/parse template-path contentResolver empty-fn-map) context (->Html)))))
      "/some/route" "query-string/query-string" {}
      "/some/route?key=value" "query-string/query-string" {:foo {:bar {:key "value"}}}
      "/some/route?key=value&key1=value1" "query-string/query-string" {:foo {:bar {:key "value" :key1 "value1"}}})))

(deftest now-default
  (testing "render to string"
    (are [timestamp-regex template-path context]
      (is (re-find timestamp-regex (renderer/render (->StringRenderer) (parser/parse template-path contentResolver empty-fn-map) context (->Html)))
          (str "Expected timestamp format in result: " (renderer/render (->StringRenderer) (parser/parse template-path contentResolver empty-fn-map) context (->Html))))
      #"20\d{2}/\d{2}/\d{2} \d{2}:\d{2}" "now/now" {}
      #"20\d{2}-\d{2}-\d{2}" "now/now-with-format-and-time-zone" {}
      ))
  (testing "render to input stream"
    (are [timestamp-regex template-path context]
      (is (re-find timestamp-regex (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer) (parser/parse template-path contentResolver empty-fn-map) context (->Html)))))
          (str "Expected timestamp format in result: " (renderer/render (->InputStreamRenderer) (parser/parse template-path contentResolver empty-fn-map) context (->Html))))
      #"20\d{2}/\d{2}/\d{2} \d{2}:\d{2}" "now/now" {}
      #"20\d{2}-\d{2}-\d{2}" "now/now-with-format" {}
      #"20\d{2}-\d{2}-\d{2}" "now/now-with-format-and-time-zone" {})))


(deftest for-values
  (are [expected input-context]
    (= (crlf->lf expected)
       (crlf->lf (String. (.readAllBytes ^InputStream (renderer/render
                                                        (->InputStreamRenderer)
                                                        (parser/parse "loop/for-values" contentResolver empty-fn-map)
                                                        input-context
                                                        (->Html)))))
       (crlf->lf (renderer/render (->StringRenderer)
                                  (parser/parse "loop/for-values" contentResolver empty-fn-map)
                                  input-context
                                  (->Html))))
    "first true false 3 0\nsecond false false 3 1\nthird false true 3 2\n" {:values (list "first" "second" "third")}
    "first true true 1 0\n" {:values (list "first")}
    "" {:values (list)}
    ))

(deftest partial-render
  (are [expected input-file input-context] (= expected (renderer/render (->PartialRenderer)
                                                                        (parser/parse input-file contentResolver empty-fn-map)
                                                                        input-context
                                                                        (->Html)))
                                           [{:type :text :value "hello world"}] "insert-value.html" {:name "world"}
                                           [{:type :text :value "hello &lt;i&gt;world&lt;i&gt;"}] "insert-value.html" {:name "<i>world<i>"}
                                           [{:type :text :value "hello World from "} {:type :value-node :value [:location]}] "if-statement.txt" {:some {:condition "wolrd"}}
                                           [{:type  :text
                                             :value "hello "}
                                            {:condition  [:some
                                                          :condition]
                                             :type       :if
                                             :when-false [{:type  :text
                                                           :value ""}]
                                             :when-true  [{:type  :text
                                                           :value "World from "}
                                                          {:type  :value-node
                                                           :value [:location]}]}] "if-statement.txt" {}
                                           [{:type :text :value "hello World! from world"}] "if-else-statement.txt" {:some {:condition true} :location "world"}
                                           [{:type  :text
                                             :value "hello "}
                                            {:condition  [:some
                                                          :condition]
                                             :type       :if
                                             :when-false [{:type  :text
                                                           :value "jj! "}
                                                          {:type  :value-node
                                                           :value [:location]}]
                                             :when-true  [{:type  :text
                                                           :value "World! from "}
                                                          {:type  :value-node
                                                           :value [:location]}]}] "if-else-statement.txt" {}

                                           [{:type  :text
                                             :value "The planets are:Planet Mercury index is 1,that makes it firstPlanet Venus index is 2Planet Earth index is 3Planet Mars index is 4Planet Jupiter index is 5Planet Saturn index is 6Planet Uranus index is 7Planet Neptune index is 8,that makes it last"}]
                                           "loop/for-loop-no-new-line" {:planets ["Mercury" "Venus" "Earth" "Mars" "Jupiter" "Saturn" "Uranus" "Neptune"]}

                                           [{:type  :text
                                             :value "/some/route?key=value"}] "query-string/query-string" {:foo {:bar {"key" "value"}}}

                                           [{:type  :text
                                             :value "The planets are:Planet Mercury. Planet Venus. Planet Earth. Planet Mars. Planet Jupiter. Planet Saturn. Planet Uranus. Planet Neptune. "}]
                                           "loop/each-loop-no-new-line" {:planets ["Mercury" "Venus" "Earth" "Mars" "Jupiter" "Saturn" "Uranus" "Neptune"]}
                                           ))

(deftest render-depth-test
  (are [expected template-path context]
    (= expected
       (renderer/render (->StringRenderer)
                        (parser/parse template-path contentResolver empty-fn-map)
                        context
                        (->Html)))
    "hello world" "path-resolver/length-1" {:value "world"}
    "hello world" "path-resolver/length-1" (Container. "world")
    "hello world1" "path-resolver/length-2" {:value1 {:value "world1"}}
    "hello world1" "path-resolver/length-2" (Container1. (Container. "world1"))
    "hello world1" "path-resolver/length-2" (Container1. {:value "world1"})
    "hello world1" "path-resolver/length-2" {:value1 (Container. "world1")}
    "hello world2" "path-resolver/length-3" {:value2 {:value1 {:value "world2"}}}
    "hello world2" "path-resolver/length-3" (Container2. (Container1. (Container. "world2")))
    "hello world3" "path-resolver/length-4" {:value3 {:value2 {:value1 {:value "world3"}}}}
    "hello world3" "path-resolver/length-4" (Container3. (Container2. (Container1. (Container. "world3"))))
    "hello world4" "path-resolver/length-5" {:value4 {:value3 {:value2 {:value1 {:value "world4"}}}}}
    "hello world4" "path-resolver/length-5" (Container4. (Container3. (Container2. (Container1. (Container. "world4")))))
    "hello world5" "path-resolver/length-6" {:value5 {:value4 {:value3 {:value2 {:value1 {:value "world5"}}}}}}
    "hello world5" "path-resolver/length-6" (Container5. (Container4. (Container3. (Container2. (Container1. (Container. "world5"))))))
    "hello world6" "path-resolver/length-7" {:value6 {:value5 {:value4 {:value3 {:value2 {:value1 {:value "world6"}}}}}}}
    "hello world6" "path-resolver/length-7" (Container6. (Container5. (Container4. (Container3. (Container2. (Container1. (Container. "world6")))))))
    ))

(deftest unclosed-tag-error-messages
  (are [expected-file input-file]
    (= (crlf->lf (slurp (io/resource expected-file)))
       (crlf->lf (String. (.readAllBytes ^InputStream (renderer/render
                                                        (->InputStreamRenderer)
                                                        (parser/parse input-file contentResolver empty-fn-map)
                                                        {}
                                                        nil))))
       (crlf->lf (renderer/render (->StringRenderer)
                                  (parser/parse input-file contentResolver empty-fn-map)
                                  {}
                                  nil
                                  )))
    "tagstack/expected-unclosed-each-tag.html" "tagstack/unclosed-each-tag"
    "tagstack/expected-unclosed-if-tag.html" "tagstack/unclosed-if-tag"
    "tagstack/expected-unclosed-for-tag.html" "tagstack/unclosed-for-tag"
    "tagstack/expected-unclosed-let-tag.html" "tagstack/unclosed-let-tag"
    ))

(deftest escape-html
  (let [context {:value "<some>tag</some>"}]
    (are [rendered-string template-path]

      (= rendered-string
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer)
                                                               (parser/parse template-path contentResolver empty-fn-map)
                                                               context
                                                               nil)))
         (renderer/render (->StringRenderer)
                          (parser/parse template-path contentResolver empty-fn-map)
                          context
                          nil))
      "&lt;some&gt;tag&lt;/some&gt;" "escape/escape-html"
       )))

(deftest escape-nil
  (let [context {:value "<some>tag</some>"}]
    (are [rendered-string template-path]

      (= rendered-string
         (String. (.readAllBytes ^InputStream (renderer/render (->InputStreamRenderer)
                                                               (parser/parse template-path contentResolver empty-fn-map)
                                                               context
                                                               (->Html))))
         (renderer/render (->StringRenderer)
                          (parser/parse template-path contentResolver empty-fn-map)
                          context
                          (->Html)))
      "<some>tag</some>" "escape/escape-nil"
      )))

(deftest partial-render-escape
  (let [context {:value "<some>tag</some>"}]
    (are [rendered-string template-path]

      (= rendered-string
         (renderer/render (->PartialRenderer)
                          (parser/parse template-path contentResolver empty-fn-map)
                          context
                          (->Html))
         )
      [{:body      [{:type  :text
                     :value "&lt;some&gt;tag&lt;/some&gt;"}]
        :sanitizer :html
        :type      :escape-block}]
      "escape/escape-html"
      )))