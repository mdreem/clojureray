(ns clojureray.test-transformations
  (:require [clojure.test :refer :all]
            [clojureray.transformations :as transformations]
            [clojureray.matrix :as matrix]
            [clojureray.vector :as vector]))

(deftest multiply-by-translation-matrix
  (testing "Test translation of point"
    (let [t (transformations/translation 5 -3 2)
          p [-3 4 5 1]]
      (is (= (matrix/multiply-vector t p) [[2.0] [1.0] [7.0] [1.0]]))))
  )
