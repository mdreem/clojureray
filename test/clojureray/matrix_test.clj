(ns clojureray.matrix-test
  (:require [clojure.test :refer :all]
            [clojureray.matrix :as matrix]
            [clojureray.vector :as vector]
            [clojureray.comparison :refer :all]))

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

(deftest compute-minor-v1
  (testing "Test computing the minor of a 3x3 matrix"
    (is (= (matrix/minor [[3 5 0] [2 -1 -7] [6 -1 5]] 1 0) 25))))

(deftest compute-minor-v2
  (testing "Test computing the minor of a 3x3 matrix"
    (is (= (matrix/minor [[3 5 0] [2 -1 -7] [6 -1 5]] 1 1) 15))))

(deftest compute-cofactor-neg
  (testing "Test computing the cofactor of a 3x3 matrix with negation"
    (is (= (matrix/cofactor [[3 5 0] [2 -1 -7] [6 -1 5]] 1 0) -25))))

(deftest compute-cofactor-pos
  (testing "Test computing the cofactor of a 3x3 matrix without negation"
    (is (= (matrix/cofactor [[3 5 0] [2 -1 -7] [6 -1 5]] 1 1) 15))))

(deftest compute-determinant-3d
  (testing "Test computing the determinant of a 3d matrix"
    (is (= (matrix/determinant [[1 2 6] [-5 8 -4] [2 6 4]]) -196))))

(deftest compute-determinant-4d
  (testing "Test computing the determinant of a 4d matrix"
    (is (= (matrix/determinant [[-2 -8 3 5] [-3 1 7 3] [1 2 -9 6] [-6 7 7 -9]]) -4071))))

(deftest compute-determinant-4d-zero
  (testing "Test computing the determinant of a 4d matrix which is zero"
    (is (= (matrix/determinant [[1 1 1 1] [1 1 1 0] [1 1 0 0] [1 1 0 0]]) 0))))

(deftest invert-4d-matrix
  (testing "Test inverting a 4d matrix version 1"
    (is (aeq (matrix/invert [[-5 2 6 -8] [1 -5 1 8] [7 7 -6 -7] [1 -3 7 4]])
             [[0.21805 0.45113 0.24060 -0.04511]
              [-0.80827 -1.45677 -0.44361 0.52068]
              [-0.07895 -0.22368 -0.05263 0.19737]
              [-0.52256 -0.81391 -0.30075 0.30639]])))

  (testing "Test inverting a 4d matrix version 2"
    (is (aeq (matrix/invert [[8 -5 9 2] [7 5 6 1] [-6 0 9 6] [-3 0 -9 -4]])
             [[-0.15385 -0.15385 -0.28205 -0.53846]
              [-0.07692 0.12308 0.02564 0.03077]
              [0.35897 0.35897 0.43590 0.92308]
              [-0.69231 -0.69231 -0.76923 -1.92308]])))

  (testing "Test inverting a 4d matrix version 3"
    (is (aeq (matrix/invert [[9 3 0 9] [-5 -2 -6 -3] [-4 9 6 4] [-7 6 6 2]])
             [[-0.04074 -0.07778 0.14444 -0.22222]
              [-0.07778 0.03333 0.36667 -0.33333]
              [-0.02901 -0.14630 -0.10926 0.12963]
              [0.17778 0.06667 -0.26667 0.33333]])))
  )
