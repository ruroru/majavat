(ns jj.majavat.cache-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [jj.majavat.cache :as cache]
            [mock-clj.core :as mock]))

(defn mock-build-renderer [filename opts]
  (fn [data] {:renderer "basic" :filename filename :data data}))

(defn mock-build-html-renderer [filename opts]
  (fn [data] {:renderer "html" :filename filename :data data}))

(defn setup-teardown [f]
  (cache/clear-cache!)
  (f))

(use-fixtures :each setup-teardown)


(deftest render-caches-by-filename
  (mock/with-mock
    [jj.majavat/build-renderer mock-build-renderer]
    (cache/render "index.html" {:data "first"})
    (cache/render "index.html" {:data "second"})
    (is (= 1 (mock/call-count jj.majavat/build-renderer)))))

(deftest render-html-caches-by-filename
  (mock/with-mock
    [jj.majavat/build-html-renderer mock-build-html-renderer]
    (cache/render-html "index.html" {:data "first"})
    (cache/render-html "index.html" {:data "second"})
    (is (= 1 (mock/call-count jj.majavat/build-html-renderer)))))

(deftest different-files-different-cache-entries
  (mock/with-mock
    [jj.majavat/build-renderer mock-build-renderer]
    (cache/render "file1.html" {})
    (cache/render "file2.html" {})
    (is (= 2 (mock/call-count jj.majavat/build-renderer)))))

(deftest options-dont-affect-cache
  (mock/with-mock
    [jj.majavat/build-renderer mock-build-renderer]
    (cache/render "index.html" {:data "x"} {:opt1 "a"})
    (cache/render "index.html" {:data "y"} {:opt2 "b"})
    (is (= 1 (mock/call-count jj.majavat/build-renderer)))))

(deftest render-returns-output
  (mock/with-mock
    [jj.majavat/build-renderer mock-build-renderer]
    (let [result (cache/render "index.html" {:key "value"})]
      (is (= "basic" (:renderer result)))
      (is (= "index.html" (:filename result)))
      (is (= {:key "value"} (:data result))))))

(deftest render-html-returns-output
  (mock/with-mock
    [jj.majavat/build-html-renderer mock-build-html-renderer]
    (let [result (cache/render-html "index.html" {:key "value"})]
      (is (= "html" (:renderer result)))
      (is (= "index.html" (:filename result)))
      (is (= {:key "value"} (:data result))))))

(deftest render-defaults-empty-options
  (mock/with-mock
    [jj.majavat/build-renderer (fn [f opts] (fn [d] opts))]
    (let [result (cache/render "index.html" {})]
      (is (= {} result)))))

(deftest clear-file-removes-from-cache
  (mock/with-mock
    [jj.majavat/build-renderer mock-build-renderer]
    (cache/render "index.html" {})
    (is (= 1 (mock/call-count jj.majavat/build-renderer)))

    (cache/clear-file! "index.html")
    (cache/render "index.html" {})
    (is (= 2 (mock/call-count jj.majavat/build-renderer)))))

(deftest clear-cache-removes-all
  (mock/with-mock
    [jj.majavat/build-renderer mock-build-renderer]
    (cache/render "file1.html" {})
    (cache/render "file2.html" {})
    (is (= 2 (mock/call-count jj.majavat/build-renderer)))

    (cache/clear-cache!)
    (cache/render "file1.html" {})
    (cache/render "file2.html" {})
    (is (= 4 (mock/call-count jj.majavat/build-renderer)))))