(ns clojureray.world-test
  (:require [clojure.test :refer :all]
            [clojureray.comparison :refer :all]
            [clojureray.world :as world]
            [clojureray.shape :as shape]
            [clojureray.ray :as ray]))

(deftest tests-helpers

  (testing "Test add shape"
    (let [shape (shape/sphere 1)
          world-with-shape (world/add-shape world/empty-world shape)]
      (is (= (:shapes world-with-shape) [shape]))
      (is (= (:lights world-with-shape) []))
      )
    )

  (testing "Test add three shapes"
    (let [sphere-1 (shape/sphere 1)
          sphere-2 (shape/sphere 2)
          sphere-3 (shape/sphere 3)
          world-with-shape (-> world/empty-world
                               (world/add-shape sphere-1)
                               (world/add-shape sphere-2)
                               (world/add-shape sphere-3))]
      (is (= (:shapes world-with-shape) [sphere-1 sphere-2 sphere-3]))
      (is (= (:lights world-with-shape) []))
      )
    )
  )


(deftest intersect-world
  (testing "Test ray intersecting two spheres"
    (let [ray (ray/ray [0.0 0.0 -5.0 1.0] [0.0 0.0 1.0 0.0])
          intersections (world/intersect-world world/default-world ray)
          t-values (mapv (fn [intersection] (:t intersection)) intersections)]
      (is (= t-values [4.0 4.5 5.5 6.0]))
      )
    )
  )

(deftest prepare-computations-of-intersections
  (let [shape (shape/sphere 1)]
    (testing "Test prepare computations")
    (let [r (ray/ray [0.0 0.0 -5.0 1.0] [0.0 0.0 1.0 0.0])
          intersection (ray/intersection 4.0 shape)
          computations (world/prepare-computations intersection r)
          {t       :t
           object  :object
           point   :point
           eyev    :eyev
           normalv :normalv} computations
          ]
      (is (= t 4.0))
      (is (= object shape))
      (is (= point [0.0 0.0 -1.0 1.0]))
      (is (= eyev [0.0 0.0 -1.0 0.0]))
      (is (= normalv [0.0 0.0 -1.0 0.0]))
      )
    )
  )
