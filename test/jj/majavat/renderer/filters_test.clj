(ns jj.majavat.renderer.filters-test
  (:require
    [jj.majavat.renderer.filters :as filters]
    [clojure.test :refer [deftest are]]))



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
  (are [expected input] (= expected (filters/file-size input))
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
  (are [expected input] (= expected (filters/get-default input ["default"]))
                        "default" nil
                        "foo" "foo"))
