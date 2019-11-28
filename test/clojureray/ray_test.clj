(ns clojureray.ray-test
  (:require [clojure.test :refer :all]
            [clojureray.ray :as ray]
            [clojureray.transformation :as transformation]
            [clojureray.comparison :refer :all]
            [clojureray.shape :as shape]
            [clojureray.matrix :as matrix]))

(deftest point-on-ray
  (let [point [2.0 3.0 4.0 1.0]
        vector [1.0 0.0 0.0 0.0]
        ray (ray/ray point vector)]
    (testing "Test point on ray at distance 0.0"
      (is (= (ray/position ray 0) [2.0 3.0 4.0 1.0])))

    (testing "Test point on ray at distance 1.0"
      (is (= (ray/position ray 1.0) [3.0 3.0 4.0 1.0])))

    (testing "Test point on ray at distance -1.0"
      (is (= (ray/position ray -1.0) [1.0 3.0 4.0 1.0])))

    (testing "Test point on ray at distance 2.5"
      (is (= (ray/position ray 2.5) [4.5 3.0 4.0 1.0])))
    )
  )

(deftest intersections-with-sphere
  (let [sphere (shape/sphere 1.0)]
    (testing "A ray intersects a sphere at two points"
      (let [res (ray/intersect sphere (ray/ray [0.0 0.0 -5.0 1.0] [0.0 0.0 1.0 0.0]))
            res_0 (get res 0)
            res_1 (get res 1)
            {t_0 :t object_0 :object} res_0
            {t_1 :t object_1 :object} res_1]
        (is (= t_0 4.0))
        (is (= t_1 6.0))
        (is (= object_0 sphere))
        (is (= object_1 sphere))
        ))

    (testing "A ray intersects a sphere at a tangent"
      (let [res (ray/intersect sphere (ray/ray [0.0 1.0 -5.0 1.0] [0.0 0.0 1.0 0.0]))
            res_0 (get res 0)
            res_1 (get res 1)
            {t_0 :t object_0 :object} res_0
            {t_1 :t object_1 :object} res_1]
        (is (= t_0 5.0))
        (is (= t_1 5.0))
        (is (= object_0 sphere))
        (is (= object_1 sphere))
        ))

    (testing "A ray misses a sphere"
      (let [res (ray/intersect sphere (ray/ray [0.0 2.0 5.0 1.0] [0.0 0.0 1.0 0.0]))]
        (is (= res []))
        ))

    (testing "A ray originates inside a sphere"
      (let [res (ray/intersect sphere (ray/ray [0.0 0.0 0.0 1.0] [0.0 0.0 1.0 0.0]))
            res_0 (get res 0)
            res_1 (get res 1)
            {t_0 :t object_0 :object} res_0
            {t_1 :t object_1 :object} res_1]
        (is (= t_0 -1.0))
        (is (= t_1 1.0))
        (is (= object_0 sphere))
        (is (= object_1 sphere))
        ))

    (testing "A sphere is behind a ray"
      (let [res (ray/intersect sphere (ray/ray [0.0 0.0 5.0 1.0] [0.0 0.0 1.0 0.0]))
            res_0 (get res 0)
            res_1 (get res 1)
            {t_0 :t object_0 :object} res_0
            {t_1 :t object_1 :object} res_1]
        (is (= t_0 -6.0))
        (is (= t_1 -4.0))
        (is (= object_0 sphere))
        (is (= object_1 sphere))
        ))
    )
  )

(deftest intersection-hits
  (let [sphere (shape/sphere 1.0)]
    (testing "The hit, when all intersections have positive t"
      (let [i1 (ray/intersection 1.0, sphere)
            i2 (ray/intersection 2.0, sphere)]
        (is (= (ray/hit [i1 i2]) i1))
        (is (= (ray/hit [i2 i1]) i1))
        )
      )

    (testing "The hit, when some intersections have negative t"
      (let [i1 (ray/intersection -1.0, sphere)
            i2 (ray/intersection 1.0, sphere)]
        (is (= (ray/hit [i1 i2]) i2))
        (is (= (ray/hit [i2 i1]) i2))
        )
      )

    (testing "The hit, when all intersections have negative t"
      (let [i1 (ray/intersection -2.0, sphere)
            i2 (ray/intersection -1.0, sphere)]
        (is (= (ray/hit [i1 i2]) nil))
        (is (= (ray/hit [i2 i1]) nil))
        )
      )

    (testing "The hit is always the lowest nonnegative intersection"
      (let [i1 (ray/intersection 5.0, sphere)
            i2 (ray/intersection 7.0, sphere)
            i3 (ray/intersection -3.0, sphere)
            i4 (ray/intersection 2.0, sphere)]
        (is (= (ray/hit [i1 i2 i3 i4]) i4))
        (is (= (ray/hit [i1 i4 i2 i3]) i4))
        )
      )
    )
  )


(deftest transform-rays
  (let [r (ray/ray [1.0 2.0 3.0 1.0] [0.0 1.0 0.0 0.0])]
    (testing "Translating a ray"
      (let [translation (ray/transform r (transformation/translation 3 4 5))]
        (is (= (ray/get-origin translation) [4.0, 6.0, 8.0, 1.0]))
        (is (= (ray/get-direction translation) [0.0, 1.0, 0.0, 0.0])))
      )

    (testing "Scaling a ray"
      (let [translation (ray/transform r (transformation/scaling 2 3 4))]
        (is (= (ray/get-origin translation) [2.0, 6.0, 12.0, 1.0]))
        (is (= (ray/get-direction translation) [0.0, 3.0, 0.0, 0.0])))
      )
    )
  )

(deftest compute-normals-to-sphere
  (testing "Test normal on sphere"
    (let [p (/ (Math/sqrt 3) 3)]
      (is (aeq (ray/normal-at (shape/sphere 1) [p p p 1.0])
               [p p p 0.0])))
    )

  (testing "Test normal on translated sphere"
    (let [translation (transformation/translation 0.0 1.0 0.0)]
      (is (aeq (ray/normal-at (transformation/set-transform (shape/sphere 1) translation) [0 1.707106 -0.707106 1.0])
               [0.0 0.707106 -0.707106 0.0]))
      )
    )

  (testing "Test normal on transformed sphere"
    (let [scaling (transformation/scaling 1.0 0.5 1.0)
          rotation (transformation/rotation_z (/ Math/PI 5))
          combined (matrix/multiply scaling rotation)
          p (/ (Math/sqrt 2) 2)]
      (is (aeq (ray/normal-at (transformation/set-transform (shape/sphere 1) combined) [0 p (- p) 1.0])
               [0.0 0.97014 -0.24254 0.0]))
      )
    )
  )

(deftest compute-reflections
  (testing "Test reflecting a factor approaching at 45 degrees"
    (is (= (ray/reflect [1.0 -1.0 0.0 0.0] [0.0 1.0 0.0 0.0]) [1.0 1.0 0.0 0.0]))
    )

  (testing "Test reflecting a vector off a slanted surface"
    (let [p (/ (Math/sqrt 2) 2)]
      (is (aeq (ray/reflect [0.0 -1.0 0.0 0.0] [p p 0.0 0.0]) [1.0 0.0 0.0 0.0]))
      ))
  )

(deftest lighting-tests
  (let [mat shape/default-material
        position [0.0 0.0 0.0 1.0]
        p (/ (Math/sqrt 2) 2)]

    (testing "Lighting with the eye between the light and the surface"
      (let [light (shape/point-light [0.0 0.0 -10.0 1.0] [1.0 1.0 1.0])
            eyev [0.0 0.0 -1.0 0.0]
            normalv [0.0 0.0 -1.0 0.0]]
        (is (aeq (ray/lighting mat light position eyev normalv false)
                 [1.9 1.9 1.9]))
        ))

    (testing "Lighting with the eye between light and surface, eye offset 45 degrees"
      (let [light (shape/point-light [0.0 0.0 -10.0 1.0] [1.0 1.0 1.0])
            eyev [0.0 p (- p) 0.0]
            normalv [0.0 0.0 -1.0 0.0]]
        (is (aeq (ray/lighting mat light position eyev normalv false)
                 [1.0 1.0 1.0]))
        ))

    (testing "Lighting with eye opposite surface, light offset 45 degrees"
      (let [light (shape/point-light [0.0 10.0 -10.0 1.0] [1.0 1.0 1.0])
            eyev [0.0 0.0 -1.0 0.0]
            normalv [0.0 0.0 -1.0 0.0]]
        (is (aeq (ray/lighting mat light position eyev normalv false)
                 [0.7364 0.7364 0.7364]))
        ))

    (testing "Lighting with eye in the path of the reflection vector"
      (let [light (shape/point-light [0.0 10.0 -10.0 1.0] [1.0 1.0 1.0])
            eyev [0.0 (- p) (- p) 0.0]
            normalv [0.0 0.0 -1.0 0.0]]
        (is (aeq (ray/lighting mat light position eyev normalv false)
                 [1.6364 1.6364 1.6364]))
        ))

    (testing "Lighting with the light behind the surface"
      (let [light (shape/point-light [0.0 0.0 10.0 1.0] [1.0 1.0 1.0])
            eyev [0.0 0.0 -1.0 0.0]
            normalv [0.0 0.0 -1.0 0.0]]
        (is (aeq (ray/lighting mat light position eyev normalv false)
                 [0.1 0.1 0.1]))
        ))
    )
  )

(deftest normalize-ray
  (let [r (ray/ray [1.0 2.0 3.0 1.0] [1.0 1.0 1.0 0.0])
        p (/ (Math/sqrt 3) 3)]
    (testing "Normalize a ray"
      (let [norm (ray/normalize r)]
        (is (= (ray/get-origin norm) [1.0 2.0 3.0 1.0]))
        (is (aeq (ray/get-direction norm) [p, p, p, 0.0])))
      )
    )
  )
