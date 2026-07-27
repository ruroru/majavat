(ns jj.majavat.build-string-renderer-test
  (:require [clojure.test :refer [deftest is testing]]
            [jj.majavat :as majavat]
            [jj.majavat.renderer.sanitizer :refer [->Html]]))

(deftest single-arity
  (testing "renders a template string with a context"
    (let [render (majavat/build-string-renderer "my template {{ value }}")]
      (is (= "my template hello" (render {:value "hello"}))))))

(deftest opts-arity
  (testing "accepts config in the second arity"
    (let [render (majavat/build-string-renderer "<b>{{ value }}</b>"
                                                {:sanitizer (->Html)})]
      (is (= "<b>&lt;script&gt;</b>" (render {:value "<script>"}))))))
