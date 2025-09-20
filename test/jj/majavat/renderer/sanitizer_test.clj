(ns jj.majavat.renderer.sanitizer-test
  (:require [clojure.test :refer [deftest are]]
            [jj.majavat.renderer.sanitizer :refer [sanitize ->Html ->Json]]))

(deftest html-test
  (are [expected sanitizer input] (= expected (sanitize sanitizer input))
                        "foo&gt;&lt;&quot;&apos;&amp;bar" (->Html) "foo><\"'&bar"
                        "\\\"\\\\ \\\\\\/ \\b \\f \\n \\r \\t" (->Json) "\"\\ \\/ \b \f \n \r \t"
                        ))