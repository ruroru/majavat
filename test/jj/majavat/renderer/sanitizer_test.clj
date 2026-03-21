(ns jj.majavat.renderer.sanitizer-test
  (:require [clojure.test :refer [deftest are]]
            [jj.majavat.protocol.renderer.sanitizer :as sanitizer]
            [jj.majavat.renderer.sanitizer :refer [->Html ->None ->Json]]))

(deftest escape
  (are [expected sanitizer input] (= expected (sanitizer/sanitize sanitizer input))
                                  "foo&gt;&lt;&quot;&apos;&amp;bar" (->Html) "foo><\"'&bar"
                                  "foo><\"'&bar" (->None) "foo><\"'&bar"
                                  "safe" (->Html) "safe"
                                  "safe" (->Json) "safe"
                                  "\\\"\\\\ \\\\\\/ \\b \\f \\n \\r \\t" (->Json) "\"\\ \\/ \b \f \n \r \t"
                                  ))