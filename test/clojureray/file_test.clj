(ns clojureray.file-test
  (:require [clojure.test :refer :all]
            [clojureray.file :as file]))

(deftest convert-creating-ppm
  (testing "Test convert canvas to string")
  (is (= (file/convert-canvas [[[1 2 3] [4 5 6]],
                               [[7 8 9] [10 11 12]]])
         "   1    2    3     4    5    6\n   7    8    9    10   11   12"))

  (testing "Test create ppm-header")
  (is (= (file/ppm-header 123 456)
         "P3\n123 456\n255"))
  )
