(ns benchmark-test
  (:require [clojure.test :refer [deftest]]
            [jj.majavat :as majavat]
            [selmer.parser :as selmer]

            [hiccup.page :as page])
  (:import (java.io  OutputStream)
           (jj.majavat.renderer InputStreamRenderer)
           (jj.majavat.stream SequentialByteArrayInputStream)))

(def pre-render-context {
                         :page-title      "Home"
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
                         :welcome-heading "Welcome to Our Blog"
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
                         :current-year    "2025"
                         })

(def iterations 1000000)

(defn hiccup-template [{:keys [page-title site-name nav-items is-home-page
                               welcome-heading welcome-message posts
                               about-content current-year]}]
  (page/html5
    [:head
     [:meta {:charset "UTF-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:title page-title]]
    [:body
     [:header
      [:h1 site-name]
      [:nav
       [:ul
        (for [item nav-items]
          [:li [:a {:href (:url item)} (:label item)]])]]]
     [:main
      (when is-home-page
        [:section#home
         [:h2 welcome-heading]
         [:p welcome-message]])
      (when posts
        [:section#blog-posts
         [:h2 "Latest Posts"]
         (for [post posts]
           [:article
            [:h3 [:a {:href (:url post)} (:title post)]]
            [:p (:excerpt post)]
            [:small "Published on " (:date post)]])])
      [:section#about
       [:h2 "About Us"]
       [:p about-content]]]
     [:footer
      [:p (str "© " current-year " . All Rights Reserved.")]]]))

(deftest test-majavat
  (let [render-fn (majavat/build-renderer "majavat.template")]
    (println (str "\nMajavat - String rendering " iterations " times:"))
    (time
      (dotimes [_ iterations]
        (render-fn pre-render-context)))))

(deftest test-majavat-is
  (let [render-fn (majavat/build-renderer "majavat.template" {:renderer (InputStreamRenderer.)})
        null-output-stream (OutputStream/nullOutputStream)]
    (println (str "\nMajavat - InputStream rendering " iterations " times:"))
    (time
      (dotimes [_ iterations]
        (.transferTo ^SequentialByteArrayInputStream (render-fn pre-render-context) null-output-stream)))))

(deftest test-selmer
  (println (str "\nSelmer - Rendering " iterations " times:"))
  (time
    (dotimes [_ iterations]
      (selmer/render-file "selmer.template" pre-render-context))))

(deftest test-hiccup
  (println (str "\nHiccup - Rendering " iterations " times:"))
  (time
    (dotimes [_ iterations]
      (hiccup-template pre-render-context))))