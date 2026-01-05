(ns jj.majavat.renderer.sanitizer-test
  (:require [clojure.test :refer [deftest are]]
            [jj.majavat.renderer.sanitizer :refer [sanitize ->Html ->Json]]))

(deftest escape
  (are [expected sanitizer input] (= expected (sanitize sanitizer input))
                                  "foo&gt;&lt;&quot;&apos;&amp;bar" (->Html) "foo><\"'&bar"
                                  "safe" (->Html) "safe"
                                  "safe" (->Json) "safe"
                                  "\\\"\\\\ \\\\\\/ \\b \\f \\n \\r \\t" (->Json) "\"\\ \\/ \b \f \n \r \t"
                                  ))