(ns clojureray.refraction-test
  (:require [clojure.test :refer :all]
            [clojureray.refraction :as refraction]
            [clojureray.shape :as shape]
            [clojureray.ray :as ray]
            [clojureray.world :as world]
            [clojureray.util :as util]))

(deftest check-at
  (testing "Find elemend at"
    (let [a {:a 1}
          b {:a 2}
          c {:a 3}
          d {:a 4}]
      (is (= (refraction/at [a b c] {:a 2}) 1))
      (is (= (refraction/at [a b c] a) 0))
      (is (= (refraction/at [a b c] b) 1))
      (is (= (refraction/at [a b c] c) 2))
      (is (= (refraction/at [a b c] d) nil))
      )
    )
  )

(deftest update-container-list
  (testing "Update containers"
    (let [a {:a 1}
          b {:a 2}
          c {:a 3}
          d {:a 4}]
      (is (= (refraction/update-containers [a b c] b) [a c]))
      (is (= (refraction/update-containers [a b c] d) [a b c d]))
      (is (= (refraction/update-containers [] a) [a]))
      )
    )
  )

(deftest processing-of-intersections
  (let [sphere-0 (-> (shape/sphere 1)
                     (shape/set-material (-> shape/default-material
                                             (shape/set-refractive-index 1.5))))
        sphere-1 (-> (shape/sphere 1)
                     (shape/set-material (-> shape/default-material
                                             (shape/set-refractive-index 2.0))))
        sphere-2 (-> (shape/sphere 1)
                     (shape/set-material (-> shape/default-material
                                             (shape/set-refractive-index 2.5))))
        hit-0 (ray/intersection 2.0 sphere-0)
        hit-1 (ray/intersection 2.75 sphere-1)
        hit-2 (ray/intersection 3.25 sphere-2)
        hit-3 (ray/intersection 4.75 sphere-1)
        hit-4 (ray/intersection 5.25 sphere-2)
        hit-5 (ray/intersection 6.0 sphere-0)
        intersections [hit-0 hit-1 hit-2 hit-3 hit-4 hit-5]]
    (is (= (refraction/process-intersections intersections hit-0) {:n1 1.0 :n2 1.5}))
    (is (= (refraction/process-intersections intersections hit-1) {:n1 1.5 :n2 2.0}))
    (is (= (refraction/process-intersections intersections hit-2) {:n1 2.0 :n2 2.5}))
    (is (= (refraction/process-intersections intersections hit-3) {:n1 2.5 :n2 2.5}))
    (is (= (refraction/process-intersections intersections hit-4) {:n1 2.5 :n2 1.5}))
    (is (= (refraction/process-intersections intersections hit-5) {:n1 1.5 :n2 1.0}))
    (is (= (refraction/process-intersections [hit-0] hit-0) {:n1 1.0 :n2 1.5}))
    )
  )

(deftest compute-refracted-color
  (testing "The refracted color with an opaque surface"
    (let [w world/default-world
          shape (get-in w [:shapes 0])
          r (ray/ray (util/point 0 0 -5) (util/ray-vector 0 0 1))
          xs [(ray/intersection 4.0 shape) (ray/intersection 6.0 shape)]
          comps (world/prepare-computations (get xs 0) r xs)
          c (refraction/refracted-color w comps 5)
          ]
      (is (= c (util/color 0 0 0)))
      )
    )

  (testing "The refracted color at the maximum recursive depth"
    (let [material (-> shape/default-material
                       (shape/set-transparency 10.0)
                       (shape/set-refractive-index 1.5))
          w (assoc-in world/default-world [:shapes 0 :material] material)
          shape (get-in w [:shapes 0])
          r (ray/ray (util/point 0 0 -5) (util/ray-vector 0 0 1))
          xs [(ray/intersection 4.0 shape) (ray/intersection 6.0 shape)]
          comps (world/prepare-computations (get xs 0) r xs)
          c (refraction/refracted-color w comps 0)
          ]
      (is (= c (util/color 0 0 0)))
      )
    )
  )
