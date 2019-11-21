(ns clojureray.ray-test
  (:require [clojure.test :refer :all]
            [clojureray.ray :as ray]))

(deftest multiply-by-translation-matrix
  (let [point [2.0 3.0 4.0 1.0]
        vector [1.0 0.0 0.0 0.0]]
    (testing "Test point on ray at distance 0.0"
      (is (= (ray/position [point vector] 0) [2.0 3.0 4.0 1.0])))

    (testing "Test point on ray at distance 1.0"
      (is (= (ray/position [point vector] 1.0) [3.0 3.0 4.0 1.0])))

    (testing "Test point on ray at distance -1.0"
      (is (= (ray/position [point vector] -1.0) [1.0 3.0 4.0 1.0])))

    (testing "Test point on ray at distance 2.5"
      (is (= (ray/position [point vector] 2.5) [4.5 3.0 4.0 1.0])))
    )
  )
