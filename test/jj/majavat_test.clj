(ns jj.majavat-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [mock-clj.core :as mock]
            [jj.majavat :as majavat]
            [jj.majavat.renderer :refer [->InputStreamRenderer ->StringRenderer]]
            [jj.majavat.parser :as parser]
            [jj.majavat.renderer.sanitizer :refer [->Html]])
  (:import (java.io InputStream)))

(defn- crlf->lf [s]
  (str/replace s "\r\n" "\n"))


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
                                             ((majavat/build-renderer file-path {:renderer (->InputStreamRenderer {})}) context))))) "verifying render to input stream")))

(deftest render-test
  (let [file-path "html/index.html"]
    (testing "Not escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
             (crlf->lf ((majavat/build-renderer file-path) context)))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-renderer file-path {:renderer (->InputStreamRenderer {})}) context))))) "verifying render to input stream"))
    (testing "Escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped.html")))
             (crlf->lf ((majavat/build-renderer file-path {:renderer (->StringRenderer {:sanitizer (->Html)})}) context)))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               ((majavat/build-renderer file-path {:renderer (->InputStreamRenderer {:sanitizer (->Html)})}) context)))))
          "verifying render to input stream"))))

(deftest render-wrong-return-type-returns-string
  (let [file-path "html/index.html"]
    (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
           (crlf->lf ((majavat/build-renderer file-path {:return-type :foo}) context))) "verifying wrong type returns string ")))

(deftest can-set-input-stream-renderer
  (let [file-path "html/index.html"]
    (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
           (crlf->lf (String. (.readAllBytes ^InputStream
                                             ((majavat/build-renderer file-path {:renderer (->InputStreamRenderer {:sanitizer nil})}) context)))))
        "verifying wrong type returns string ")))




(deftest try-rendering-file-that-does-not-exist
  (is (= (crlf->lf (slurp (io/resource "render-template-not-found.html")))
         (crlf->lf ((majavat/build-renderer "not-existing-file" {}) {}))) "verifying failure to render string")
  (is (= (crlf->lf (slurp (io/resource "render-template-not-found.html")))
         (crlf->lf (String. (.readAllBytes ^InputStream
                                           ((majavat/build-renderer "not-existing-file" {:renderer (->InputStreamRenderer {})}) {}))))) "verifying failure to render input stream"))


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