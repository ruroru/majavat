(ns jj.majavat-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.majavat :as majavat]
            [jj.majavat.parser :as parser]
            [jj.majavat.renderer.sanitizer :refer [->Html]]
            [mock-clj.core :as mock])
  (:import (java.io InputStream)))

(defn- crlf->lf [s]
  (str/replace s "\r\n" "\n"))


(deftest render-error-test
  (let [file-path "html/index-with-error.html"
        file-content {}]
    (is (= (crlf->lf (slurp (io/resource "html/expected-error.html")))
           (crlf->lf (majavat/render file-path file-content {})))
        "verifying render to string")
    (is (= (crlf->lf (slurp (io/resource "html/expected-error.html")))
           (crlf->lf (String. (.readAllBytes ^InputStream
                                             (majavat/render file-path file-content {:return-type :input-stream}))))) "verifying render to input stream")))

(deftest render-test
  (let [file-path "html/index.html"
        file-content {
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
                      :current-year    2025}]
    (testing "Not escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
             (crlf->lf (majavat/render file-path file-content {})))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               (majavat/render file-path file-content {:return-type :input-stream}))))) "verifying render to input stream"))
    (testing "Escaped html"
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped.html")))
             (crlf->lf (majavat/render file-path file-content {:sanitizer (->Html)})))
          "verifying render to string")
      (is (= (crlf->lf (slurp (io/resource "html/expected-escaped.html")))
             (crlf->lf (String. (.readAllBytes ^InputStream
                                               (majavat/render file-path file-content {:return-type    :input-stream
                                                                                            :sanitizer (->Html)})))))
          "verifying render to input stream"))))

(deftest cache-enabled-test
  (mock/with-mock
    [parser/parse [{:type  :text
                    :value "text"}]]
    (majavat/render "somefile" {})
    (majavat/render "somefile" {})
    (majavat/render "somefile" {})
    (is (= 1 (mock/call-count parser/parse)))))

(deftest cache-disabled-test
  (mock/with-mock
    [parser/parse [{:type  :text
                    :value "text"}]]
    (majavat/render "somefile" {} {:cache? false})
    (majavat/render "somefile" {} {:cache? false})
    (majavat/render "somefile" {} {:cache? false})
    (is (= 3 (mock/call-count parser/parse)))))


(deftest try-rendering-file-that-does-not-exist
  (is (= (crlf->lf (slurp (io/resource "render-template-not-found.html")))
         (crlf->lf (majavat/render "not-existing-file" {}))) "verifying failure to render string")
  (is (= (crlf->lf (slurp (io/resource "render-template-not-found.html")))
         (crlf->lf (String. (.readAllBytes ^InputStream
                                           (majavat/render "not-existing-file" {} {:return-type :input-stream}))))) "verifying failure to render input stream"))