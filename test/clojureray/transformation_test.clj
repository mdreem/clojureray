(ns clojureray.transformation_test
  (:require [clojure.test :refer :all]
            [clojureray.transformation :as transformation]
            [clojureray.matrix :as matrix]
            [clojureray.ray :as ray]
            [clojureray.comparison :refer :all]
            [clojureray.shape :as shape]))

(deftest multiply-by-translation-matrix
  (testing "Test translation of point"
    (let [t (transformation/translation 5 -3 2)
          p [-3 4 5 1]]
      (is (= (matrix/multiply-vector t p) [[2.0] [1.0] [7.0] [1.0]]))))

  (testing "Test inverse of a translation of point"
    (let [t (matrix/invert (transformation/translation 5 -3 2))
          p [-3 4 5 1]]
      (is (= (matrix/multiply-vector t p) [[-8.0] [7.0] [3.0] [1.0]]))))

  (testing "Test translation of vector"
    (let [t (transformation/translation 5 -3 2)
          v [-3 4 5 0]]
      (is (= (matrix/multiply-vector t v) [[-3.0] [4.0] [5.0] [0.0]]))))
  )

(deftest multiply-by-scaling-matrix
  (testing "Test scaling matrix applied to a point"
    (let [s (transformation/scaling 2 3 4)
          p [-4 6 8 1]]
      (is (= (matrix/multiply-vector s p) [[-8.0] [18.0] [32.0] [1.0]]))))

  (testing "Test applying an inverse scaling matrix applied to a point"
    (let [s (matrix/invert (transformation/scaling 2 3 4))
          p [-4 6 8 1]]
      (is (= (matrix/multiply-vector s p) [[-2.0] [2.0] [2.0] [1.0]]))))

  (testing "Test reflection of a point"
    (let [s (matrix/invert (transformation/scaling -1 1 1))
          p [2 3 4 1]]
      (is (= (matrix/multiply-vector s p) [[-2.0] [3.0] [4.0] [1.0]]))))

  (testing "Test scaling matrix applied to a vector"
    (let [s (transformation/scaling 2 3 4)
          v [-4 6 8 0]]
      (is (= (matrix/multiply-vector s v) [[-8.0] [18.0] [32.0] [0.0]]))))
  )

(deftest multiply-by-rotation-matrix
  (testing "Test scaling matrix applied to a point - half quarter - x-axis"
    (let [r (transformation/rotation_x (/ Math/PI 4))
          p [0 1 0 1]]
      (is (aeq (matrix/multiply-vector r p) [[0.0] [0.707107] [0.707107] [1.0]]))))

  (testing "Test scaling matrix applied to a point - full quarter - x-axis"
    (let [r (transformation/rotation_x (/ Math/PI 2))
          p [0 1 0 1]]
      (is (aeq (matrix/multiply-vector r p) [[0.0] [0.0] [1.0] [1.0]]))))

  (testing "Test scaling matrix applied to a point - half quarter - y-axis"
    (let [r (transformation/rotation_y (/ Math/PI 4))
          p [0 0 1 1]]
      (is (aeq (matrix/multiply-vector r p) [[0.707107] [0.0] [0.707107] [1.0]]))))

  (testing "Test scaling matrix applied to a point - full quarter - y-axis"
    (let [r (transformation/rotation_y (/ Math/PI 2))
          p [0 0 1 1]]
      (is (aeq (matrix/multiply-vector r p) [[1.0] [0.0] [0.0] [1.0]]))))


  (testing "Test scaling matrix applied to a point - half quarter - z-axis"
    (let [r (transformation/rotation_z (/ Math/PI 4))
          p [0 1 0 1]]
      (is (aeq (matrix/multiply-vector r p) [[-0.707107] [0.707107] [0.0] [1.0]]))))

  (testing "Test scaling matrix applied to a point - full quarter - z-axis"
    (let [r (transformation/rotation_z (/ Math/PI 2))
          p [0 1 0 1]]
      (is (aeq (matrix/multiply-vector r p) [[-1.0] [0.0] [0.0] [1.0]]))))
  )

(deftest multiply-by-shearing-matrix
  (testing "Test sharing matrix applied to a point - x_y"
    (let [s (transformation/shearing 1.0 0.0 0.0 0.0 0.0 0.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[5.0] [3.0] [4.0] [1.0]]))))

  (testing "Test sharing matrix applied to a point - x_x"
    (let [s (transformation/shearing 0.0 1.0 0.0 0.0 0.0 0.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[6.0] [3.0] [4.0] [1.0]]))))

  (testing "Test sharing matrix applied to a point - y_x"
    (let [s (transformation/shearing 0.0 0.0 1.0 0.0 0.0 0.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[2.0] [5.0] [4.0] [1.0]]))))

  (testing "Test sharing matrix applied to a point - y_z"
    (let [s (transformation/shearing 0.0 0.0 0.0 1.0 0.0 0.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[2.0] [7.0] [4.0] [1.0]]))))

  (testing "Test sharing matrix applied to a point - z_x"
    (let [s (transformation/shearing 0.0 0.0 0.0 0.0 1.0 0.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[2.0] [3.0] [6.0] [1.0]]))))

  (testing "Test sharing matrix applied to a point - z_y"
    (let [s (transformation/shearing 0.0 0.0 0.0 0.0 0.0 1.0)
          p [2 3 4 1]]
      (is (aeq (matrix/multiply-vector s p) [[2.0] [3.0] [7.0] [1.0]]))))
  )

(deftest multiply-by-chained-transformation-matrices
  (let [a (transformation/rotation_x (/ Math/PI 2))
        b (transformation/scaling 5 5 5)
        c (transformation/translation 10 5 7)
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

(deftest intersect-with-translations
  (testing "Intersecting a scaled sphere with a ray"
    (let [r (ray/ray [0.0 0.0 -5.0 1.0] [0.0 0.0 1.0 0.0])
          s (shape/sphere 1.0)
          t (transformation/scaling 2.0 2.0 2.0)
          transformed_s (transformation/set-transform s t)
          intersections (ray/intersect transformed_s r)]
      (is (= (:transformation transformed_s) t))
      (is (= (count intersections) 2))
      (is (= (:t (get intersections 0)) 3.0))
      (is (= (:t (get intersections 1)) 7.0))

      )
    )
  )