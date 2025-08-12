(ns jj.majavat-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [jj.majavat :as majavat]))

(defn- crlf->lf [s]
  (str/replace s "\r\n" "\n"))

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
                      :current-year    2025
                      }]
    (is (=
          (crlf->lf (slurp (io/resource "html/expected.html")))
          (crlf->lf (majavat/render-file file-path file-content))))))