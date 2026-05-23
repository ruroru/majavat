(ns jj.majavat-test
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jj.majavat :as majavat]
            [jj.majavat.parser :as parser]
            [jj.majavat.renderer :refer [->InputStreamRenderer ->StringRenderer]]
            [jj.majavat.protocol.renderer.sanitizer :as sanitizer]
            [jj.majavat.renderer.sanitizer :refer [->Html]]
            [jj.majavat.resolver.fs :refer [->FsResolver]]

            [mock-clj.core :as mock])
  (:import (java.io InputStream)
           (jj.majavat.protocol.dictionary Dictionary)))


(defrecord MockDictionary []
  Dictionary
  (translate [_ language word]
    (get-in {:en {:hello "hello" :world "world"}
             :fi {:hello "hei" :world "maailma"}}
            [language word])))


(defn- crlf->lf [s]
  (str/replace s "\r\n" "\n"))

(defn- materialize [result]
  (if (instance? InputStream result)
    (String. (.readAllBytes ^InputStream result))
    result))

(defn- render-as-string [file-path opts ctx]
  (crlf->lf (materialize ((majavat/build-renderer file-path opts) ctx))))

(def ^:private expected
  (memoize (fn [resource-path]
             (crlf->lf (slurp (io/resource resource-path))))))

(defn- label-of [& variants]
  (str/join " / " (map :label variants)))

(def filters {:italic (fn [value]
                        (format "<i>%s</i>" value))})

(defrecord StarSanitizer []
  sanitizer/Sanitizer
  (sanitize [_ input]
    (when input
      (str/replace input #"\S" "*"))))

(def context
  {:locale          :fi
   :page-title      "Home - My Awesome Blog"
   :site-name       "My Awesome Blog"
   :nav-items       [{:url "/" :label "Home"}
                     {:url "/about" :label "About"}
                     {:url "/contact" :label "Contact"}]
   :is-home-page    true
   :welcome-heading "Welcome to Our Blog!"
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
   :current-year    2025
   :empty-list      []
   :csrf-token      "csrf-abc123"
   :params          (sorted-map :page 1 :q "hello world")})

(def ^:private renderer-axis
  [{:label "StringRenderer" :opts {:renderer (->StringRenderer)}}
   {:label "InputStreamRenderer" :opts {:renderer (->InputStreamRenderer)}}])

(def ^:private cache-axis
  [{:label "cached" :opts {}}
   {:label ":cache? false" :opts {:cache? false}}
   {:label ":cache? nil" :opts {:cache? nil}}])

(defn- context-axis
  [splittable-keys ctx]
  (let [singletons (map vector splittable-keys)
        full (when (next splittable-keys) [splittable-keys])
        splits (concat [[]] singletons full)]
    (for [split splits]
      (if (seq split)
        {:label (str "pre-render " (pr-str (vec split)))
         :opts  {:pre-render (select-keys ctx split)}
         :ctx   (apply dissoc ctx split)}
        {:label "no pre-render" :opts {} :ctx ctx}))))

(def ^:private test-resources-root
  (.getAbsolutePath (io/file "test/resources")))

(defn- fs-path
  [path]
  (when path
    (let [abs (.getAbsolutePath (io/file test-resources-root path))]
      (if (.exists (io/file abs)) abs path))))

(def ^:private resolver-axis
  [{:label "ResourceResolver" :opts {} :path-fn identity}
   {:label "FsResolver" :opts {:template-resolver (->FsResolver)} :path-fn fs-path}])

(def ^:private splittable-keys
  [:page-title :site-name :welcome-heading :current-year :is-home-page
   :nav-items :posts])

(def ^:private templates
  [{:label           "plain"
    :file-path       "html/index.html"
    :expected        "html/expected.html"
    :context         context
    :splittable-keys splittable-keys
    :opts            {:environment {:dictionary (->MockDictionary)}}}
   {:label           "html-escaped"
    :file-path       "html/index.html"
    :expected        "html/expected-escaped.html"
    :context         context
    :splittable-keys splittable-keys
    :opts            {:environment {:dictionary (->MockDictionary)} :sanitizer (->Html)}}
   {:label           "custom filter"
    :file-path       "html/index-with-custom-filter.html"
    :expected        "html/expected-custom-filter.html"
    :context         context
    :splittable-keys splittable-keys
    :opts            {:environment {:filters filters :dictionary (->MockDictionary)}}}
   {:label           "custom filter + html-escaped"
    :file-path       "html/index-with-custom-filter.html"
    :expected        "html/expected-escaped-custom-filter.html"
    :context         context
    :splittable-keys splittable-keys
    :opts            {:environment {:filters filters :dictionary (->MockDictionary)} :sanitizer (->Html)}}
   {:label           "custom sanitizer record"
    :file-path       "html/customfilters/template.html"
    :expected        "html/customfilters/expected.html"
    :context         {:value "a blast"}
    :splittable-keys [:value]
    :opts            {:environment {:sanitizers {:starnitizer (->StarSanitizer)}}}}
   {:label           "parse error"
    :file-path       "html/index-with-error.html"
    :expected        "html/expected-error.html"
    :context         {}
    :splittable-keys []
    :opts            {}}
   {:label           "missing template file"
    :file-path       "not-existing-file"
    :expected        "render-template-not-found.html"
    :context         {}
    :splittable-keys []
    :opts            {}}
   {:label           "nil template path"
    :file-path       nil
    :expected        "render-template-nil.html"
    :context         {}
    :splittable-keys []
    :opts            {}}])

(deftest render-test
  (doseq [t templates
          [r c res split] (combo/cartesian-product
                            renderer-axis
                            cache-axis
                            resolver-axis
                            (context-axis (:splittable-keys t) (:context t)))]
    (testing (label-of t r c res split)
      (is (= (expected (:expected t))
             (render-as-string ((:path-fn res) (:file-path t))
                               (merge (:opts t) (:opts split) (:opts r) (:opts c) (:opts res))
                               (:ctx split)))))))

(deftest idempotence-test
  (doseq [t templates
          [r c] (combo/cartesian-product renderer-axis cache-axis)]
    (testing (label-of t r c)
      (let [render-fn (majavat/build-renderer (:file-path t)
                                              (merge (:opts t) (:opts r) (:opts c)))
            results (repeatedly 3 #(crlf->lf (materialize (render-fn (:context t)))))]
        (is (apply = results)
            "render-fn must return identical output across repeated calls")))))

(deftest invalid-pre-render-falls-back-test
  (let [file-path "html/index.html"
        expected-output (expected "html/expected-without-title.html")
        ctx (dissoc context :page-title)
        invalid-cases [{:label ":pre-render []" :opts {:pre-render [] :environment {:dictionary (->MockDictionary)}}}
                       {:label ":pre-render nil" :opts {:pre-render nil :environment {:dictionary (->MockDictionary)}}}]]
    (doseq [[bad r c] (combo/cartesian-product invalid-cases renderer-axis cache-axis)]
      (testing (label-of bad r c)
        (is (= expected-output
               (render-as-string file-path
                                 (merge (:opts bad) (:opts r) (:opts c))
                                 ctx)))))))

(deftest cache-call-count-test
  (let [cases [{:label ":cache? false" :opts {:cache? false} :expected-calls 3}
               {:label ":cache? nil" :opts {:cache? nil} :expected-calls 3}
               {:label ":cache? true" :opts {} :expected-calls 1}]]
    (doseq [{:keys [label opts expected-calls]} cases]
      (testing label
        (mock/with-mock
          [parser/parse [{:type :text :value "text"}]]
          (let [render-fn (majavat/build-renderer "somefile" opts)]
            (render-fn {}) (render-fn {}) (render-fn {}))
          (is (= expected-calls (mock/call-count parser/parse))))))))

(deftest nil-options-default-correctly
  (let [file-path "html/index.html"
        expected-output (expected "html/expected.html")
        cases [{:label ":renderer nil" :opts {:renderer nil :environment {:dictionary (->MockDictionary)}}}
               {:label ":template-resolver nil" :opts {:template-resolver nil :environment {:dictionary (->MockDictionary)}}}
               {:label ":return-type :foo" :opts {:return-type :foo :environment {:dictionary (->MockDictionary)}}}
               {:label "config map nil" :opts {:environment {:dictionary (->MockDictionary)}}}]]
    (doseq [{:keys [label opts]} cases]
      (testing label
        (is (= expected-output
               (render-as-string file-path opts context)))))))


