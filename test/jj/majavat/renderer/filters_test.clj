(ns jj.majavat.renderer.filters-test
  (:require
    [clojure.test :refer [are deftest]]
    [jj.majavat.renderer.filters :as filters])
  (:import (java.net URI URL)
           (java.time LocalDate LocalDateTime LocalTime ZoneId ZonedDateTime)
           (java.util UUID)))



(deftest upper-roman-test
  (are [expected input] (= expected (filters/upper-roman input))
                        "I" "i"
                        "V" "v"
                        "X" "x"
                        "L" "l"
                        "C" "c"
                        "D" "d"
                        "M" "m"
                        "IV" "iv"
                        "IX" "ix"
                        "XL" "xl"
                        "XC" "xc"
                        "CD" "cd"
                        "CM" "cm"
                        "XIV" "xiv"
                        "XLIV" "xliv"
                        "MCMXC" "mcmxc"
                        "MMXXIV" "mmxxiv"
                        "MCDXLIV" "mcdxliv"
                        "abc" "abc"
                        "IvX" "IvX"
                        "123iv" "123iv"))

(deftest file-size
  (are [expected input] (= expected (filters/file-size input ))
                        nil nil
                        nil "not-a-number"
                        nil "1024"
                        nil :keyword
                        nil []
                        nil {}
                        nil true
                        nil false
                        "1 byte" 1
                        "0 bytes" 0
                        "2 bytes" 2
                        "10 bytes" 10
                        "100 bytes" 100
                        "1023 bytes" 1023
                        "-1 bytes" -1
                        "-10 bytes" -10
                        "-100 bytes" -100
                        "1.1 KB" 1111
                        "1.5 KB" 1536
                        "2 KB" 2048
                        "10 KB" 10240
                        "100 KB" 102400
                        "1023 KB" 1047552
                        "1023.9 KB" 1048474
                        "1 MB" 1048576
                        "1.1 MB" 1153434
                        "2 MB" 2097152
                        "10 MB" 10485760
                        "100 MB" 104857600
                        "500.5 MB" 524820480
                        "1023 MB" 1072693248
                        "1 GB" 1073741824
                        "1.5 GB" 1610612736
                        "2 GB" 2147483648
                        "10 GB" 10737418240
                        "100 GB" 107374182400
                        "1023 GB" 1098437885952
                        "1 TB" 1099511627776
                        "1.5 TB" 1649267441664
                        "2 TB" 2199023255552
                        "10 TB" 10995116277760
                        "100 TB" 109951162777600
                        "1 PB" 1125899906842624
                        "1.5 PB" 1688849860263936
                        "2 PB" 2251799813685248
                        "10 PB" 11258999068426240
                        "1023 bytes" 1023
                        "1 KB" 1024
                        "1024.0 KB" 1048575
                        "1 MB" 1048576
                        "1.1 KB" 1126
                        "2.3 MB" 2411724
                        "3.7 GB" 3971577692
                        "4.2 TB" 4618441900646
                        "1023.0 TB" 1124774005170176
                        "1000 PB" 1125899906842624000))

(deftest as-int
  (are [expected input] (= expected (filters/as-int input))
                        nil nil
                        1 "1"
                        1025 "1025"))


(deftest as-long
  (are [expected input] (= expected (filters/as-long input))
                        nil nil
                        1 "1"
                        1025 "1025"))

(deftest title-case
  (are [expected input] (= expected (filters/title-case input))
                        "Foo Bar-Baz Qux. Quux" "foo bar-baz qux. quux"))

(deftest get-default
  (are [expected input] (= expected (filters/get-default input "default"))
                        "default" nil
                        "foo" "foo"))

(deftest local-date->date
  (are [expected input] (= expected (filters/->formatted-local-date (LocalDate/of 2022, 01, 01) [input]))
                        "2022-01-01" "yyyy-MM-dd"
                        "2022" "yyyy"
                        "2022-01-01" "not-avalid-format"
                        ))
(deftest local-date-time->date
  (are [expected input] (= expected (filters/->formatted-local-date-time (LocalDateTime/of 2022, 01, 01, 1, 1) [input]))
                        "2022-01-01 01:01" "yyyy-MM-dd hh:mm"
                        "2022" "yyyy"
                        "2022-01-01T01:01" "not-avalid-format"
                        ))

(deftest local-time->date
  (are [expected input] (= expected (filters/->formatted-local-time (LocalTime/of 11, 11) [input]))
                        "11 --- 11" "hh --- mm"
                        "11" "hh"
                        "11:11" "not-avalid-format"
                        ))

(deftest zoned-date-time
  (are [expected input] (= expected (filters/->formatted-zoned-date-time (ZonedDateTime/of (LocalDateTime/of 2022, 01, 02, 03, 04) (ZoneId/of "UTC")) input))
                        "03 --- 04" ["hh --- mm"]
                        "03" ["hh"]
                        "2022-01-02T03:04Z[UTC]" ["not-avalid-format"]
                        "12 --- 04" ["hh --- mm" "Asia/Tokyo"]
                        ))


(deftest instant-date-time
  (let [test-instant (.toInstant (ZonedDateTime/of (LocalDateTime/of 2022, 01, 02, 03, 04) (ZoneId/of "UTC")))]
    (are [expected input] (= expected (filters/->formatted-instant test-instant input))
                          "2022 --- 01 04" ["yyyy --- MM mm"]
                          "2022 - 04" ["yyyy - mm"]
                          (str test-instant) ["not-avalid-format"]
                          "12 --- 12:04" ["hh --- hh:mm" "Asia/Tokyo"])))

(deftest test-uuid-as-string
  (are [expected input] (= expected (filters/->uuid-as-string input))
                        "550e8400-e29b-41d4-a716-446655440000" (UUID/fromString "550e8400-e29b-41d4-a716-446655440000")))

(deftest url
  (are [expected input] (= expected (filters/->url-to-string input))
                        "http://www.example.com" (URL. "http://www.example.com")))

(deftest uri
  (are [expected input] (= expected (filters/->uri-to-string input))
                        "/some/path" (URI. "/some/path")))

(deftest map-where
  (are [expected input key value] (= expected (filters/->handle-where input key value))
                                  (list
                                    {:key "y" :random "d" :value "d"}
                                    {:key "y" :random "f" :value "t"}
                                    {:key "y" :random "i" :value "r"})
                                  [
                                   {:key "a" :value "b" :random "c"}
                                   {:key "y" :value "d" :random "d"}
                                   {:key "b" :value "b" :random "e"}
                                   {:key "y" :value "t" :random "f"}
                                   {:key "d" :value "t" :random "g"}
                                   {:key "d" :value "b" :random "h"}
                                   {:key "y" :value "r" :random "i"}]
                                  :key "y"))
(deftest round
  (are [input expected] (= expected (filters/round-number input))
                        1 1
                        1.4 1
                        1.5 2
                        1.6 2
                        2 2
                        2.0 2))

(deftest floor-test
  (are [input expected] (= expected (filters/get-floor input))
                        1 1.0
                        1.4 1.0
                        1.5 1.0
                        1.6 1.0
                        2 2.0
                        2.0 2.0))

(deftest ceil-test
  (are [input expected] (= expected (filters/get-ceiling input))
                        1 1.0
                        1.4 2.0
                        1.5 2.0
                        1.6 2.0
                        2 2.0
                        2.0 2.0))

(deftest abs-test
  (are [input expected] (= expected (filters/get-absolute-value input))
                        1 1.0
                        0 0.0
                        -1 1.0))
(deftest append
  (are [input arg expected] (= expected (filters/append input arg))
                            "foo" "bar" "foobar"
                            "foo" nil "foo"

                            ))

(deftest prepend
  (are [input arg expected] (= expected (filters/prepend input arg))
                            "foo" "bar" "barfoo"
                            "foo" nil "foo"
                            ))
(deftest slugify
  (are [input expected] (= expected (filters/slugify input))
                        "foo" "foo"
                        "Foo" "foo"
                        "foo bar baz" "foo-bar-baz"
                        "FooBar-Baz_123" "foobar-baz_123"
                        ))

(deftest first-test
  (are [input expected] (= expected (filters/get-first input))
                        (list :a :b) :a
                        {:a :b :c :d} [:a :b]
                        ))

(deftest rest-test
  (are [input expected] (= expected (filters/get-rest input))
                        (list :a :b) (list :b)
                        {:a :b :c :d} {:c :d}
                        ))
