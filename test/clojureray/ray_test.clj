(ns clojureray.ray-test
  (:require [clojure.test :refer :all]
            [clojureray.ray :as ray]
            [clojureray.comparison :refer :all]
            [clojureray.shape :as shape]))

(deftest point-on-ray
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
