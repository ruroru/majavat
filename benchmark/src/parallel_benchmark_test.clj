(ns parallel-benchmark-test
  (:require [clojure.test :refer [deftest]]
            [jj.majavat :as majavat]
            [selmer.parser :as selmer]
            [hiccup.page :as page])
  (:import (java.io OutputStream)
           (java.util.concurrent Executors CountDownLatch)
           (jj.majavat.renderer InputStreamRenderer)
           (jj.majavat.stream SequentialByteArrayInputStream)))

(def pre-render-context
  {:page-title      "Home"
   :site-name       "My Awesome Blog"
   :nav-items       [{:url "/" :label "Home"}
                     {:url "/about" :label "About"}
                     {:url "/contact" :label "Contact"}]
   :is-home-page    true
   :welcome-heading "Welcome to Our Blog"
   :welcome-message "Stay up-to-date with our latest articles and insights."
   :posts           [{:url     "/posts/first-post"
                      :title   "Getting Started with Web Development"
                      :excerpt "A beginner's guide to the fundamentals of HTML, CSS, and JavaScript."
                      :date    "August 12, 2025"}
                     {:url     "/posts/second-post"
                      :title   "The Power of Templating Engines"
                      :excerpt "Learn how templating engines can streamline your web development workflow."
                      :date    "August 10, 2025"}]
   :about-content   "We are a team of passionate developers dedicated to sharing knowledge about modern web technologies."
   :current-year    "2025"})

(def iterations 100000)
(def thread-counts [1 2 4 8])
(def warmup-iterations 70000)

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

(defn run-parallel
  "Runs work-fn total-iterations times across thread-count threads.
   Returns elapsed wall-clock time in milliseconds."
  [thread-count total-iterations work-fn]
  (let [executor (Executors/newFixedThreadPool thread-count)
        per-thread (quot total-iterations thread-count)
        done-latch (CountDownLatch. thread-count)
        start-latch (CountDownLatch. 1)]
    (try
      (dotimes [_ thread-count]
        (.submit executor
                 ^Runnable
                 (fn []
                   (.await start-latch)
                   (try
                     (dotimes [_ per-thread]
                       (work-fn))
                     (finally
                       (.countDown done-latch))))))
      (let [start (System/nanoTime)]
        (.countDown start-latch)
        (.await done-latch)
        (/ (- (System/nanoTime) start) 1e6))
      (finally
        (.shutdown executor)))))


(defn runv-virtual-parallel
  "Runs work-fn total-iterations times across thread-count threads.
   Returns elapsed wall-clock time in milliseconds."
  [thread-count total-iterations work-fn]
  (let [executor (Executors/newFixedThreadPool thread-count)
        per-thread (quot total-iterations thread-count)
        done-latch (CountDownLatch. thread-count)
        start-latch (CountDownLatch. 1)]
    (try
      (dotimes [_ thread-count]
        (.submit executor
                 ^Runnable
                 (fn []
                   (.await start-latch)
                   (try
                     (dotimes [_ per-thread]
                       (work-fn))
                     (finally
                       (.countDown done-latch))))))
      (let [start (System/nanoTime)]
        (.countDown start-latch)
        (.await done-latch)
        (/ (- (System/nanoTime) start) 1e6))
      (finally
        (.shutdown executor)))))
(defn sleep []
  (Thread/sleep 1000))

(defn benchmark-engine
  "Runs work-fn at each thread count and prints a comparison table."
  [engine-name work-fn]
  (println (format "\n=== %s (%,d total iterations) ===" engine-name iterations))
  (println (format "  %-10s %-15s %-15s %-10s" "Threads" "Time (ms)" "Ops/sec" "Speedup"))
  (println (str "  " (apply str (repeat 55 \-))))
  ;; JIT warmup
  (run-parallel 1 warmup-iterations work-fn)
  (sleep)
  (let [baseline (atom nil)]
    (doseq [threads thread-counts]
      (let [elapsed-ms (run-parallel threads iterations work-fn)
            ops-per-sec (/ iterations (/ elapsed-ms 1000.0))]
        (when (= threads 1) (reset! baseline elapsed-ms))
        (println (format "  %-10d %-15.2f %-15.0f %.2fx"
                         threads
                         elapsed-ms
                         ops-per-sec
                         (double (/ @baseline elapsed-ms))))))))



(defn benchmark-virtual-engine
  "Runs work-fn at each thread count and prints a comparison table."
  [engine-name work-fn]
  (println (format "\n=== %s (%,d total iterations) ===" engine-name iterations))
  (println (format "  %-10s %-15s %-15s %-10s" "Threads" "Time (ms)" "Ops/sec" "Speedup"))
  (println (str "  " (apply str (repeat 55 \-))))
  ;; JIT warmup
  (runv-virtual-parallel 1 warmup-iterations work-fn)
  (sleep)
  (let [baseline (atom nil)]
    (doseq [threads thread-counts]
      (let [elapsed-ms (runv-virtual-parallel threads iterations work-fn)
            ops-per-sec (/ iterations (/ elapsed-ms 1000.0))]
        (when (= threads 1) (reset! baseline elapsed-ms))
        (println (format "  %-10d %-15.2f %-15.0f %.2fx"
                         threads
                         elapsed-ms
                         ops-per-sec
                         (double (/ @baseline elapsed-ms))))))))

(deftest test-majavat-parallel
  (let [render-fn (majavat/build-renderer "majavat.template")]
    (benchmark-engine "Majavat - String"
                      (fn [] (render-fn pre-render-context)))))

(deftest test-majavat-is-parallel
  (let [render-fn (majavat/build-renderer "majavat.template"
                                          {:renderer (InputStreamRenderer.)})
        null-output-stream (OutputStream/nullOutputStream)]
    (benchmark-engine "Majavat - InputStream"
                      (fn []
                        (.transferTo ^SequentialByteArrayInputStream
                                     (render-fn pre-render-context)
                                     null-output-stream)))))


(deftest test-v-thread-majavat-parallel
  (let [render-fn (majavat/build-renderer "majavat.template")]
    (benchmark-virtual-engine "Majavat - String - virtual thread"
                              (fn [] (render-fn pre-render-context)))))

(deftest test-v-thread-majavat-is-parallel
  (let [render-fn (majavat/build-renderer "majavat.template"
                                          {:renderer (InputStreamRenderer.)})
        null-output-stream (OutputStream/nullOutputStream)]
    (benchmark-virtual-engine "Majavat - InputStream - virtual thread"
                              (fn []
                                (.transferTo ^SequentialByteArrayInputStream
                                             (render-fn pre-render-context)
                                             null-output-stream)))))


(deftest test-selmer-parallel
  (benchmark-engine "Selmer"
                    (fn [] (selmer/render-file "selmer.template" pre-render-context))))

(deftest test-v-threads-selmer-parallel
  (benchmark-virtual-engine "Selmer - virtual-threads"
                    (fn [] (selmer/render-file "selmer.template" pre-render-context))))

(deftest test-hiccup-parallel
  (benchmark-engine "Hiccup"
                    (fn [] (hiccup-template pre-render-context))))

(deftest test-v-threads-hiccup-parallel
  (benchmark-virtual-engine "Hiccup - virtual threads"
                    (fn [] (hiccup-template pre-render-context))))