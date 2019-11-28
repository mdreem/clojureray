(ns clojureray.file-test
  (:require [clojure.test :refer :all]
            [clojureray.file :as file]))

(deftest convert-creating-ppm
  (testing "Test convert canvas to string")
  (is (= (file/convert-canvas [[[0.1 0.2 0.3] [0.4 0.5 0.6]],
                               [[0.7 0.8 0.9] [0.10 0.11 0.12]]])
         "  25   51   76   102  127  153\n 178  204  229    25   28   30"))

  (testing "Test create ppm-header")
  (is (= (file/ppm-header 123 456)
         "P3\n123 456\n255"))
  )
