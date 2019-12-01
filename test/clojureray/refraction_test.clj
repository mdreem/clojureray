(ns clojureray.refraction-test
  (:require [clojure.test :refer :all]
            [clojureray.comparison :refer :all]
            [clojureray.refraction :as refraction]
            [clojureray.shape :as shape]
            [clojureray.ray :as ray]
            [clojureray.world :as world]
            [clojureray.util :as util]
            [clojureray.pattern :as pattern]
            [clojureray.transformation :as transformation]))

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
          c (world/refracted-color w comps 5)
          ]
      (is (= c (util/color 0 0 0)))
      )
    )

  (testing "The refracted color at the maximum recursive depth"
    (let [material (-> shape/default-material
                       (shape/set-transparency 1.0)
                       (shape/set-refractive-index 1.5))
          w (assoc-in world/default-world [:shapes 0 :material] material)
          shape (get-in w [:shapes 0])
          r (ray/ray (util/point 0 0 -5) (util/ray-vector 0 0 1))
          xs [(ray/intersection 4.0 shape) (ray/intersection 6.0 shape)]
          comps (world/prepare-computations (get xs 0) r xs)
          c (world/refracted-color w comps 0)]
      (is (= c (util/color 0 0 0)))
      )
    )

  (testing "The refracted color under total reflection"
    (let [p (/ (Math/sqrt 2) 2)
          material (-> shape/default-material
                       (shape/set-transparency 1.0)
                       (shape/set-refractive-index 1.5))
          w (assoc-in world/default-world [:shapes 0 :material] material)
          shape (get-in w [:shapes 0])
          r (ray/ray (util/point 0 0 p) (util/ray-vector 0 1 0))
          i-0 (ray/intersection (- p) shape)
          i-1 (ray/intersection p shape)
          xs [i-0 i-1]
          comps (world/prepare-computations i-1 r xs)
          c (world/refracted-color w comps 5)]
      (is (= c (util/color 0 0 0)))
      )
    )

  (testing "The refracted color with a refracted ray"
    (let [material-0 (-> shape/default-material
                         (shape/set-color (pattern/test-pattern (clojureray.transformation/scaling 1 2 3)))
                         (shape/set-ambient 1.0))
          material-1 (-> shape/default-material
                         (shape/set-transparency 1.0)
                         (shape/set-refractive-index 1.5))
          w (-> world/default-world
                (assoc-in [:shapes 0 :material] material-0)
                (assoc-in [:shapes 1 :material] material-1))
          shape-0 (get-in w [:shapes 0])
          shape-1 (get-in w [:shapes 1])
          r (ray/ray (util/point 0 0 0.1) (util/ray-vector 0 1 0))
          i-0 (ray/intersection -0.9899 shape-0)
          i-1 (ray/intersection -0.4899 shape-1)
          i-2 (ray/intersection 0.4899 shape-1)
          i-3 (ray/intersection 0.9899 shape-0)
          xs [i-0 i-1 i-2 i-3]
          comps (world/prepare-computations i-2 r xs)
          c (world/refracted-color w comps 5)]
      (is (aeq c (util/color 0 0.99888 0.04725)))
      )
    )

  (testing "shade-hit with a transparent material"
    (let [p (/ (Math/sqrt 2) 2)
          material-floor (-> shape/default-material
                             (shape/set-transparency 0.5)
                             (shape/set-refractive-index 1.5))
          material-sphere (-> shape/default-material
                              (shape/set-color (util/constant-color 1 0 0))
                              (shape/set-ambient 0.5))
          floor (-> shape/plane
                    (shape/set-transformation (transformation/translation 0 -1 0))
                    (shape/set-material material-floor))
          ball (-> (shape/sphere 1)
                   (shape/set-transformation (transformation/translation 0 -3.5 -0.5))
                   (shape/set-material material-sphere))
          w (-> world/default-world
                (world/add-shape floor)
                (world/add-shape ball))
          r (ray/ray (util/point 0 0 -3) (util/ray-vector 0 (- p) p))
          intersection (ray/intersection (Math/sqrt 2) floor)
          xs [intersection]
          comps (world/prepare-computations intersection r xs)
          c (world/shade-hit w comps 5)]
      (is (aeq c (util/color 0.936425 0.686425 0.686425)))
      )
    )
  )
