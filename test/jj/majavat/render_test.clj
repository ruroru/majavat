(ns jj.majavat.render-test
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [deftest is are]]
    [jj.majavat.parser :as parser]
    [jj.majavat.renderer :as renderer]
    [jj.majavat.renderer.ops.html :as hops]
    [jj.majavat.resolver.fs :as fcr]
    [jj.majavat.resolver.resource :as rcr]
    )
  (:import (java.io InputStream)))


(defn- crlf->lf [s]
  (str/replace s "\r\n" "\n"))

(def contentResolver (rcr/->ResourceResolver))

(deftest prerender-test
  (is (= [{:type :text :value "hello world"}
          {:type :value-node :value :not-existing}]
         (renderer/pre-render [{:type :text :value "hello "}
                               {:type :value-node :value :name}
                               {:type :value-node :value :not-existing}]
                              {:name "world"}))))


(deftest prerender-empty-context-test
  (is (= [{:type :text :value "hello "}
          {:type :value-node :value :name}
          {:type :value-node :value :age}]
         (renderer/pre-render [{:type :text :value "hello "}
                               {:type :value-node :value :name}
                               {:type :value-node :value :age}]
                              {}))))

(deftest prerender-no-text-nodes-test
  (is (= [{:type :value-node :value :missing}
          {:type :value-node :value :also-missing}]
         (renderer/pre-render [{:type :value-node :value :name}
                               {:type :value-node :value :missing}
                               {:type :value-node :value :age}
                               {:type :value-node :value :also-missing}]
                              {:name "Alice" :age 30}))))

(deftest prerender-all-values-exist-test
  (is (= [{:type :text :value "Hello Alice, You are 30"}]
         (renderer/pre-render [{:type :text :value "Hello "}
                               {:type :value-node :value :name}
                               {:type :text :value ", You are "}
                               {:type :value-node :value :age}]
                              {:name "Alice" :age 30}))))

(deftest prerender-mixed-node-types-test
  (is (= [{:type :text :value "Department: Engineering"}
          {:type :for :identifier :employee :source [:dept :employees]}
          {:type :value-node :value :missing-budget}
          {:type :if :condition [:dept :active]}]
         (renderer/pre-render [{:type :text :value "Department: "}
                               {:type :value-node :value :name}
                               {:type :for :identifier :employee :source [:dept :employees]}
                               {:type :value-node :value :missing-budget}
                               {:type :if :condition [:dept :active]}]
                              {:name "Engineering" :budget "$500K"}))))



(deftest prerender-special-characters-test
  (is (= [{:type :text :value "🎉 Hello 世界!"}
          {:type :value-node :value :missing}]
         (renderer/pre-render [{:type :text :value "🎉 Hello "}
                               {:type :value-node :value :greeting}
                               {:type :text :value "!"}
                               {:type :value-node :value :missing}]
                              {:greeting "世界"}))))

(deftest prerender-nested-context-keys-test
  (is (= [{:type :text :value "User: admin"}
          {:type :value-node :value :role}                  ; This should remain since we only check top-level
          {:type :value-node :value :missing}]
         (renderer/pre-render [{:type :text :value "User: "}
                               {:type :value-node :value :username}
                               {:type :value-node :value :role} ; nested key, should remain
                               {:type :value-node :value :missing}]
                              {:username "admin"
                               :profile  {:role "administrator"}}))))

(deftest prerender-empty-instructions-test
  (is (= [] (renderer/pre-render [] {:name "test"}))))

(deftest prerender-keyword-vs-string-keys-test
  (is (= [{:type :text :value "Hello "}
          {:type :value-node :value :name}                  ; keyword not found in string-key context
          {:type :value-node :value :missing}]
         (renderer/pre-render [{:type :text :value "Hello "}
                               {:type :value-node :value :name}
                               {:type :value-node :value :missing}]
                              {"name" "Alice"}))))          ; string key, not keyword

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
    (are [input expected] (= (format "hello %s" expected) (renderer/render template {:name input} {:escape? true
                                                                                                   :ops     (hops/->HtmlOps)}))
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
