(ns jj.majavat.fragment-test
  (:require [clojure.test :refer [deftest is]]
            [jj.majavat.parser :as parser]
            [jj.majavat.renderer :refer [->StringRenderer]]
            [jj.majavat.renderer.sanitizer :refer [->None]]
            [jj.majavat.renderer.json :refer [->DefaultJsonSerializer]]
            [jj.majavat.protocol.mock-dictionary :refer [create-mock-dictionary]]
            [jj.majavat.protocol.renderer.render-target :as rt]
            [jj.majavat.error-handler.fail-fast :as fail-fast]
            [jj.majavat :as majavat]
            [jj.majavat.resolver.resource :as rcr]))

(def ^:private resolver (rcr/->ResourceResolver))
(def ^:private dict (create-mock-dictionary))
(def ^:private json-ser (->DefaultJsonSerializer))

(defn- build-file [file fragment]
  (parser/parse-template file resolver {} {} dict (->None) json-ser fragment))

(defn- build [fragment]
  (build-file "fragment/page.html" fragment))

(defn- render [template context]
  (rt/render (->StringRenderer) template context (fail-fast/->FailFast)))

(deftest whole-page-splices-fragment-body-inline
  (is (= "<html><li>Bob</li><footer>bye</footer></html>"
         (render (build nil) {:name "Bob"}))))

(deftest select-fragment-returns-only-body
  (is (= "<li>Bob</li>"
         (render (build :row) {:name "Bob"}))))

(deftest unknown-fragment-yields-error-map
  (is (= {:type          "fragment-not-found-error"
          :error-message "fragment 'missing' not found"}
         (build :missing))))

(deftest build-html-renderer-fragment-opt
  (let [render-fn (majavat/build-html-renderer "fragment/page.html" {:fragment :row})]
    (is (= "<li>Bob</li>" (render-fn {:name "Bob"}))))
  (let [render-fn (majavat/build-html-renderer "fragment/page.html")]
    (is (= "<html><li>Bob</li><footer>bye</footer></html>" (render-fn {:name "Bob"})))))

;; Fragment declared deeply nested: let -> for -> if -> fragment.
;; select-fragment recurses through every control-block body, so the fragment
;; is both selectable by name and unwrapped for the whole-page render.
(deftest deeply-nested-fragment-is-selectable-by-name
  ;; The selected body renders standalone; the enclosing let/for/if are gone,
  ;; so only the item's own fields matter here.
  (is (= "<li>Bob</li>"
         (render (build-file "fragment/in-if.html" :row) {:item {:name "Bob"}}))))

(deftest deeply-nested-fragment-renders-inline-in-whole-page
  (is (= "<html><li>Bob</li><footer>bye</footer></html>"
         (render (build-file "fragment/in-if.html" nil)
                 {:items [{:name "Bob" :show true}
                          {:name "Al" :show false}]})))
  ;; with no matching items the fragment body simply never renders, as usual
  (is (= "<html><footer>bye</footer></html>"
         (render (build-file "fragment/in-if.html" nil)
                 {:items [{:name "Al" :show false}]}))))

;; A fragment nested inside another fragment. Both wrappers must resolve, and
;; the inner one must be selectable directly.
(deftest nested-fragments-resolve-at-every-level
  (is (= "AOI!O2B" (render (build-file "fragment/nested.html" nil) {:x "!"})))
  (is (= "OI!O2"   (render (build-file "fragment/nested.html" :outer) {:x "!"})))
  (is (= "I!"      (render (build-file "fragment/nested.html" :inner) {:x "!"}))))

;; Macros are expanded before fragments are selected, so a macro call inside a
;; fragment body works whether the whole page or just the fragment is rendered.
(deftest macro-call-inside-fragment
  (is (= "X[hi al!]Y" (render (build-file "fragment/macro-in-frag.html" nil) {:name "al"})))
  (is (= "[hi al!]"   (render (build-file "fragment/macro-in-frag.html" :f) {:name "al"}))))

;; A fragment inside a for loop renders once per item in the whole page, and
;; renders a single time (against the given context) when selected.
(deftest fragment-inside-for-loop
  (is (= "U<li>a</li><li>b</li>V"
         (render (build-file "fragment/in-for.html" nil) {:items ["a" "b"]})))
  (is (= "<li>z</li>"
         (render (build-file "fragment/in-for.html" :row) {:item "z"}))))

(deftest empty-fragment
  (is (= "PQ" (render (build-file "fragment/empty.html" nil) {})))
  (is (= ""   (render (build-file "fragment/empty.html" :empty) {}))))

;; Two fragments sharing a name make selection ambiguous; like a redefined
;; macro, this is a syntax error (reported at the second definition's line).
(deftest duplicate-fragment-name-is-an-error
  (is (= {:type          "syntax-error"
          :error-message "fragment 'dup' is already defined"
          :line          "1"}
         (build-file "fragment/dup.html" :dup)))
  (is (= {:type          "syntax-error"
          :error-message "fragment 'dup' is already defined"
          :line          "1"}
         (build-file "fragment/dup.html" nil))))
