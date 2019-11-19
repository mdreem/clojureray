(ns clojureray.vector_test
  (:require [clojure.test :refer :all]
            [clojureray.vector :as vector]))

(deftest abs-greater-than-zero
  (testing "Test abs greater than zero"
    (is (= (vector/abs 2) 2))))

(deftest abs-less-than-zero
  (testing "Test abs less than zero"
    (is (= (vector/abs -2) 2))))

(deftest compare-float-equal
  (testing "Test compare floats almost equal"
    (is (= (vector/float= 2 2.000001) true))))

(deftest compare-float-unequal
  (testing "Test compare floats unequal"
    (is (= (vector/float= 2 2.00001) false))))

(deftest addition
  (testing "Test addition"
    (is (= (vector/add [1 2 3] [4 5 6]) [5 7 9]))))

(deftest subtraction
  (testing "Test subtraction"
    (is (= (vector/subtract [1 2 3] [4 5 6]) [-3 -3 -3]))))

(deftest negation
  (testing "Test negation"
    (is (= (vector/negate [1 -2 3]) [-1 2 -3]))))

(deftest negation
  (testing "Test negation"
    (is (= (vector/negate [1 -2 3]) [-1 2 -3]))))

(deftest scalar-multiplication
  (testing "Test scalar multiplication"
    (is (= (vector/scalar-multiplication 0.5 [1 -2 3]) [0.5 -1.0 1.5]))))

(deftest vector-multiplication
  (testing "Test vector multiplication"
    (is (= (vector/squared-length [1 2 3]) 14))))

(deftest length-1
  (testing "Test length of a vector of length 1"
    (is (= (vector/length [1 0 0]) 1.0))))

(deftest length-sqrt-14
  (testing "Test length of a vector of length sqrt(14)"
    (is (= (vector/float= (vector/length [1 -2 3]) 3.741657) true))))

(deftest dot-product
  (testing "Test dot product"
    (is (= (vector/dot [1 2 3] [2 3 4]) 20))))

(deftest cross-product-v1
  (testing "Test cross product v1"
    (is (= (vector/cross [1 2 3] [2 3 4]) [-1 2 -1]))))

(deftest cross-product-v2
  (testing "Test cross product v2"
    (is (= (vector/cross [2 3 4] [1 2 3]) [1 -2 1]))))
