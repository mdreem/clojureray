(ns clojureray.test-transformations
  (:require [clojure.test :refer :all]
            [clojureray.transformations :as transformations]
            [clojureray.matrix :as matrix]
            [clojureray.vector :as vector]
            [clojureray.comparison :refer :all]))

(deftest multiply-by-translation-matrix
  (testing "Test translation of point"
    (let [t (transformations/translation 5 -3 2)
          p [-3 4 5 1]]
      (is (= (matrix/multiply-vector t p) [[2.0] [1.0] [7.0] [1.0]]))))

  (testing "Test inverse of a translation of point"
    (let [t (matrix/invert (transformations/translation 5 -3 2))
          p [-3 4 5 1]]
      (is (= (matrix/multiply-vector t p) [[-8.0] [7.0] [3.0] [1.0]]))))

  (testing "Test translation of vector"
    (let [t (transformations/translation 5 -3 2)
          v [-3 4 5 0]]
      (is (= (matrix/multiply-vector t v) [[-3.0] [4.0] [5.0] [0.0]]))))
  )

(deftest multiply-by-scaling-matrix
  (testing "Test scaling matrix applied to a point"
    (let [s (transformations/scaling 2 3 4)
          p [-4 6 8 1]]
      (is (= (matrix/multiply-vector s p) [[-8.0] [18.0] [32.0] [1.0]]))))

  (testing "Test applying an inverse scaling matrix applied to a point"
    (let [s (matrix/invert (transformations/scaling 2 3 4))
          p [-4 6 8 1]]
      (is (= (matrix/multiply-vector s p) [[-2.0] [2.0] [2.0] [1.0]]))))

  (testing "Test reflection of a point"
    (let [s (matrix/invert (transformations/scaling -1 1 1))
          p [2 3 4 1]]
      (is (= (matrix/multiply-vector s p) [[-2.0] [3.0] [4.0] [1.0]]))))

  (testing "Test scaling matrix applied to a vector"
    (let [s (transformations/scaling 2 3 4)
          v [-4 6 8 0]]
      (is (= (matrix/multiply-vector s v) [[-8.0] [18.0] [32.0] [0.0]]))))
  )

(deftest multiply-by-rotation-matrix
  (testing "Test scaling matrix applied to a point - half quarter - x-axis"
    (let [r (transformations/rotation_x (/ Math/PI 4))
          p [0 1 0 1]]
      (is (aeq (matrix/multiply-vector r p) [[0.0] [0.707107] [0.707107] [1.0]]))))

  (testing "Test scaling matrix applied to a point - full quarter - x-axis"
    (let [r (transformations/rotation_x (/ Math/PI 2))
          p [0 1 0 1]]
      (is (aeq (matrix/multiply-vector r p) [[0.0] [0.0] [1.0] [1.0]]))))

  (testing "Test scaling matrix applied to a point - half quarter - y-axis"
    (let [r (transformations/rotation_y (/ Math/PI 4))
          p [0 0 1 1]]
      (is (aeq (matrix/multiply-vector r p) [[0.707107] [0.0] [0.707107] [1.0]]))))

  (testing "Test scaling matrix applied to a point - full quarter - y-axis"
    (let [r (transformations/rotation_y (/ Math/PI 2))
          p [0 0 1 1]]
      (is (aeq (matrix/multiply-vector r p) [[1.0] [0.0] [0.0] [1.0]]))))


  (testing "Test scaling matrix applied to a point - half quarter - z-axis"
    (let [r (transformations/rotation_z (/ Math/PI 4))
          p [0 1 0 1]]
      (is (aeq (matrix/multiply-vector r p) [[-0.707107] [0.707107] [0.0] [1.0]]))))

  (testing "Test scaling matrix applied to a point - full quarter - z-axis"
    (let [r (transformations/rotation_z (/ Math/PI 2))
          p [0 1 0 1]]
      (is (aeq (matrix/multiply-vector r p) [[-1.0] [0.0] [0.0] [1.0]]))))
  )

(deftest multiply-by-shearing-matrix
  (testing "Test sharing matrix applied to a point - x_y"
    (let [s (transformations/shearing 1.0 0.0 0.0 0.0 0.0 0.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[5.0] [3.0] [4.0] [1.0]]))))

  (testing "Test sharing matrix applied to a point - x_x"
    (let [s (transformations/shearing 0.0 1.0 0.0 0.0 0.0 0.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[6.0] [3.0] [4.0] [1.0]]))))

  (testing "Test sharing matrix applied to a point - y_x"
    (let [s (transformations/shearing 0.0 0.0 1.0 0.0 0.0 0.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[2.0] [5.0] [4.0] [1.0]]))))

  (testing "Test sharing matrix applied to a point - y_z"
    (let [s (transformations/shearing 0.0 0.0 0.0 1.0 0.0 0.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[2.0] [7.0] [4.0] [1.0]]))))

  (testing "Test sharing matrix applied to a point - z_x"
    (let [s (transformations/shearing 0.0 0.0 0.0 0.0 1.0 0.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[2.0] [3.0] [6.0] [1.0]]))))

  (testing "Test sharing matrix applied to a point - z_y"
    (let [s (transformations/shearing 0.0 0.0 0.0 0.0 0.0 1.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[2.0] [3.0] [7.0] [1.0]]))))
  )

(deftest multiply-by-chained-transformation-matrices
  (let [a (transformations/rotation_x (/ Math/PI 2))
        b (transformations/scaling 5 5 5)
        c (transformations/translation 10 5 7)
        p [1 0 1 1]
        p2 (matrix/multiply-vector a p)
        p3 (matrix/multiply-vector b (first (matrix/transpose p2)))
        p4 (matrix/multiply-vector c (first (matrix/transpose p3)))
        combined (matrix/multiply c (matrix/multiply b a))
        ]
    (testing "Test apply rotation first"
      (is (aeq p2 [[1.0] [-1.0] [0.0] [1.0]])))

    (testing "Test apply scaling second"
      (is (aeq p3 [[5.0] [-5.0] [0.0] [1.0]])))

    (testing "Test apply translation third"
      (is (aeq p4 [[15.0] [0.0] [7.0] [1.0]])))

    (testing "Test apply combined"
      (is (aeq (matrix/multiply-vector combined p) [[15.0] [0.0] [7.0] [1.0]])))
    )
  )
