(ns jj.majavat-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is are testing]]
            [criterium.core :as criterium]
            [jj.majavat :as majavat]
            [jj.majavat.parser :as parser]
            [jj.majavat.renderer :refer [->InputStreamRenderer ->StringRenderer]]
            [jj.majavat.renderer.sanitizer :refer [->Html ->Json]]
            [mock-clj.core :as mock])
  (:import (java.io InputStream)
           (jj.majavat.renderer.sanitizer Sanitizer)))

(defn- crlf->lf [s]
  (str/replace s "\r\n" "\n"))


(def filters {:italic (fn [value args]
                        (format "<i>%s</i>" value))})

(def pre-render-context-with-html-chars {
                                         :page-title      "</b>Home</b> - My Awesome Blog"
                                         :welcome-heading "</>Welcome to Our Blog!</i>"
                                         :current-year    2025})

(def pre-render-context {
                         :site-name       "My Awesome Blog"
                         :nav-items       [
                                           {
                                            :url   "/"
                                            :label "Home"
                                            }
                                           {
                                            :url   "/about"
                                            :label "About"
                                            }
                                           {
                                            :url   "/contact"
                                            :label "Contact"
                                            }
                                           ]
                         :is-home-page    true
                         :welcome-message "Stay up-to-date with our latest articles and insights."
                         :posts           [
                                           {
                                            :url     "/posts/first-post"
                                            :title   "Getting Started with Web Development"
                                            :excerpt "A beginner's guide to the fundamentals of HTML, CSS, and JavaScript."
                                            :date    "August 12, 2025"
                                            }
                                           {
                                            :url     "/posts/second-post"
                                            :title   "The Power of Templating Engines"
                                            :excerpt "Learn how templating engines can streamline your web development workflow."
                                            :date    "August 10, 2025"
                                            }
                                           ]
                         :about-content   "We are a team of passionate developers dedicated to sharing knowledge about modern web technologies."
                         })

(def context {
              :page-title      "Home - My Awesome Blog"
              :site-name       "My Awesome Blog"
              :nav-items       [
                                {
                                 :url   "/"
                                 :label "Home"
                                 }
                                {
                                 :url   "/about"
                                 :label "About"
                                 }
                                {
                                 :url   "/contact"
                                 :label "Contact"
                                 }
                                ]
              :is-home-page    true
              :welcome-heading "Welcome to Our Blog!"
              :welcome-message "Stay up-to-date with our latest articles and insights."
              :posts           [
                                {
                                 :url     "/posts/first-post"
                                 :title   "Getting Started with Web Development"
                                 :excerpt "A beginner's guide to the fundamentals of HTML, CSS, and JavaScript."
                                 :date    "August 12, 2025"
                                 }
                                {
                                 :url     "/posts/second-post"
                                 :title   "The Power of Templating Engines"
                                 :excerpt "Learn how templating engines can streamline your web development workflow."
                                 :date    "August 10, 2025"
                                 }
                                ]
              :about-content   "We are a team of passionate developers dedicated to sharing knowledge about modern web technologies."
              :current-year    2025})

(deftest render-error-test
  (let [file-path "html/index-with-error.html"
        context {}]
    (is (= (crlf->lf (slurp (io/resource "html/expected-error.html")))
           (crlf->lf ((majavat/build-renderer file-path {}) context)))
        "verifying render to string")
    (is (= (crlf->lf (slurp (io/resource "html/expected-error.html")))
           (crlf->lf (String. (.readAllBytes ^InputStream
                                             ((majavat/build-renderer file-path {
                                                                                 :renderer (->InputStreamRenderer)}) context))))) "verifying render to input stream")))

(deftest render-test
  (let [file-path "html/index.html"]
    (testing "Not escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
             (crlf->lf ((majavat/build-renderer file-path) context)))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-renderer file-path {
                                                                                   :renderer (->InputStreamRenderer)}) context))))) "verifying render to input stream"))
    (testing "Escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped.html")))
             (crlf->lf ((majavat/build-renderer file-path {:sanitizer (->Html)
                                                           :renderer  (->StringRenderer)}) context)))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-renderer file-path {:sanitizer (->Html)
                                                                                   :renderer  (->InputStreamRenderer)}) context)))))
          "verifying render to input stream"))))


(deftest render-custom-filter-test
  (let [file-path "html/index-with-custom-filter.html"]
    (testing "Not escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected-custom-filter.html")))
             (crlf->lf ((majavat/build-renderer file-path {:filters filters}) context)))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected-custom-filter.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-renderer file-path {:filters  filters
                                                                                   :renderer (->InputStreamRenderer)}) context))))) "verifying render to input stream"))
    (testing "Escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped-custom-filter.html")))
             (crlf->lf ((majavat/build-renderer file-path {:filters   filters
                                                           :sanitizer (->Html)
                                                           :renderer  (->StringRenderer)}) context)))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped-custom-filter.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-renderer file-path {:filters   filters
                                                                                   :sanitizer (->Html)
                                                                                   :renderer  (->InputStreamRenderer)}) context)))))
          "verifying render to input stream"))))

(deftest render-wrong-return-type-returns-string
  (let [file-path "html/index.html"]
    (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
           (crlf->lf ((majavat/build-renderer file-path {:return-type :foo}) context))) "verifying wrong type returns string ")))

(deftest can-set-input-stream-renderer
  (let [file-path "html/index.html"]
    (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
           (crlf->lf (String. (.readAllBytes ^InputStream
                                             ((majavat/build-renderer file-path {:renderer (->InputStreamRenderer)}) context)))))
        "verifying wrong type returns string ")))




(deftest try-rendering-file-that-does-not-exist
  (is (= (crlf->lf (slurp (io/resource "render-template-not-found.html")))
         (crlf->lf ((majavat/build-renderer "not-existing-file" {}) {}))) "verifying failure to render string")
  (is (= (crlf->lf (slurp (io/resource "render-template-not-found.html")))
         (crlf->lf (String. (.readAllBytes ^InputStream
                                           ((majavat/build-renderer "not-existing-file" {:renderer (->InputStreamRenderer)}) {}))))) "verifying failure to render input stream"))


(deftest cache-disabled-test
  (mock/with-mock
    [parser/parse [{:type  :text
                    :value "text"}]]
    (let [render-fn (majavat/build-renderer "somefile" {:cache? false})]
      (render-fn {})
      (render-fn {})
      (render-fn {}))
    (is (= 3 (mock/call-count parser/parse)))))

(deftest cache-enabled-test
  (mock/with-mock
    [parser/parse [{:type  :text
                    :value "text"}]]
    (let [render-fn (majavat/build-renderer "somefile" {})]
      (render-fn {})
      (render-fn {})
      (render-fn {}))
    (is (= 1 (mock/call-count parser/parse)))))

(deftest try-rendering-nil
  (is (= (crlf->lf (slurp (io/resource "render-template-nil.html")))
         (crlf->lf ((majavat/build-renderer nil {}) {}))) "verifying failure to render string"))

(deftest renderer-set-to-nil-defaults-to-string-renderer
  (let [file-path "html/index.html"]
    (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
           (crlf->lf ((majavat/build-renderer file-path {:renderer nil}) context)))
        "verifying wrong type returns string ")))

(deftest template-resolver-set-to-nil-defaults-to-resource-resolver
  (let [file-path "html/index.html"]
    (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
           (crlf->lf ((majavat/build-renderer file-path {:template-resolver nil}) context)))
        "verifying wrong type returns string ")))

(deftest template-config-is-set-to-nil
  (let [file-path "html/index.html"]
    (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
           (crlf->lf ((majavat/build-renderer file-path nil) context))))))

(deftest cache-set-to-nil-disables
  (mock/with-mock
    [parser/parse [{:type  :text
                    :value "text"}]]
    (let [render-fn (majavat/build-renderer "somefile" {:cache? nil})]
      (render-fn {})
      (render-fn {})
      (render-fn {}))
    (is (= 3 (mock/call-count parser/parse)))))

(deftest pre-render
  (let [file-path "html/index.html"]
    (testing "Not escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
             (crlf->lf ((majavat/build-renderer file-path {:pre-render {:page-title "Home - My Awesome Blog"}})
                        (dissoc context :page-title))))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-renderer file-path {:pre-render {:page-title "Home - My Awesome Blog"}
                                                                                   :renderer   (->InputStreamRenderer)})
                                                (dissoc context :page-title)))))) "verifying render to input stream"))
    (testing "Escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped.html")))
             (crlf->lf ((majavat/build-renderer file-path {:sanitizer  (->Html)
                                                           :pre-render {:page-title "Home - My Awesome Blog"}
                                                           :renderer   (->StringRenderer)})
                        (dissoc context :page-title))))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-renderer file-path {:sanitizer  (->Html)
                                                                                   :pre-render {:page-title "Home - My Awesome Blog"}
                                                                                   :renderer   (->InputStreamRenderer)})
                                                (dissoc context :page-title))))))
          "verifying render to input stream"))))

(deftest performance-test
  (if false
    (let [file-path "html/index.html"
          render-fn (majavat/build-renderer file-path)]
      (criterium/bench (render-fn context)))))

(deftest render-html-test
  (let [file-path "html/index.html"]
    (testing "Escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped-html.html")))
             (crlf->lf ((majavat/build-html-renderer file-path {:pre-render pre-render-context-with-html-chars
                                                                :renderer   (->StringRenderer)})
                        pre-render-context)))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped-html.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-html-renderer
                                                  file-path
                                                  {:pre-render pre-render-context-with-html-chars
                                                   :renderer   (->InputStreamRenderer)})
                                                pre-render-context)))))
          "verifying render to input stream"))
    (testing "Overwrites existing sanitizer"
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped-html.html")))
             (crlf->lf ((majavat/build-html-renderer file-path {:pre-render pre-render-context-with-html-chars
                                                                :renderer   (->StringRenderer)})
                        pre-render-context)))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped-html.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-html-renderer
                                                  file-path
                                                  {:sanitizer  (->Json)
                                                   :pre-render pre-render-context-with-html-chars
                                                   :renderer   (->InputStreamRenderer)})
                                                pre-render-context)))))
          "verifying render to input stream"))))


(deftest render-not-cached-html-test
  (let [file-path "html/index.html"]
    (testing "Escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped-html.html")))
             (crlf->lf ((majavat/build-html-renderer file-path {:pre-render pre-render-context-with-html-chars
                                                                :cache?     false
                                                                :renderer   (->StringRenderer)})
                        pre-render-context)))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped-html.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-html-renderer
                                                  file-path
                                                  {:pre-render pre-render-context-with-html-chars
                                                   :cache?     false
                                                   :renderer   (->InputStreamRenderer)})
                                                pre-render-context)))))
          "verifying render to input stream"))
    (testing "Overwrites existing sanitizer"
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped-html.html")))
             (crlf->lf ((majavat/build-html-renderer file-path {:pre-render pre-render-context-with-html-chars
                                                                :cache?     false
                                                                :renderer   (->StringRenderer)})
                        pre-render-context)))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped-html.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-html-renderer
                                                  file-path
                                                  {:sanitizer  (->Json)
                                                   :cache?     false
                                                   :pre-render pre-render-context-with-html-chars
                                                   :renderer   (->InputStreamRenderer)})
                                                pre-render-context)))))
          "verifying render to input stream"))))

(deftest pre-render-faulty
  (let [file-path "html/index.html"]
    (testing "Not escaped html"
      (are [pre-render-value] (= (crlf->lf (slurp (io/resource "html/expected-without-title.html")))
                                 (crlf->lf ((majavat/build-renderer file-path {:pre-render pre-render-value})
                                            (dissoc context :page-title))))

                              []
                              nil))))

(defrecord StarSanitizer []
  Sanitizer
  (sanitize [_ input]
    (when input
      (clojure.string/replace input #"\S" "*"))))



(deftest custom-santizer
  (let [file-path "html/customfilters/template.html"]
    (testing "Not escaped html"
      (are [value] (= (crlf->lf (slurp (io/resource "html/customfilters/expected.html")))
                                 (crlf->lf ((majavat/build-renderer file-path {:sanitizers {:starnitizer (StarSanitizer.)}})
                                            {:value value})))

                              "a blast"
                              ))))
