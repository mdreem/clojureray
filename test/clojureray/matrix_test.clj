(ns clojureray.matrix-test
  (:require [clojure.test :refer :all]
            [clojureray.matrix :as matrix]))

(deftest transpose-matrix
  (testing "Test transposing a matrix"
    (is (= (matrix/transpose [[1 2 3]
                              [4 5 6]
                              [7 8 9]]) [[1 4 7]
                                         [2 5 8]
                                         [3 6 9]]))))

(deftest multiply-matrix
  (testing "Test multiplying a matrix"
    (is (= (matrix/multiply [[1 2 3 4] [5 6 7 8] [9 8 7 6] [5 4 3 2]]
                            [[-2 1 2 3] [3 2 1 -1] [4 3 6 5] [1 2 7 8]]) [[20 22 50 48]
                                                                          [44 54 114 108]
                                                                          [40 58 110 102]
                                                                          [16 26 46 42]]))))

(deftest multiply-matrix-with-id
  (testing "Test multiplying a matrix with id"
    (is (= (matrix/multiply matrix/id-4 [[1 2 3 4] [5 6 7 8] [9 8 7 6] [5 4 3 2]])
           [[1 2 3 4] [5 6 7 8] [9 8 7 6] [5 4 3 2]]))))

(deftest get-submatrix-v1
  (testing "Test getting a submatrix"
    (is (= (matrix/submatrix [[1 5 0] [-3 2 7] [0 6 -3]] 0 2)
           [[-3 2] [0 6]]))))

(deftest get-submatrix-v2
  (testing "Test getting a submatrix-v2"
    (is (= (matrix/submatrix [[-6 1 1 6] [-8 5 8 6] [-1 0 8 2] [-7 1 -1 1]] 2 1)
           [[-6 1 6] [-8 8 6] [-7 -1 1]]))))
