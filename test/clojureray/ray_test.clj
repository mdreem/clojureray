(ns clojureray.ray-test
  (:require [clojure.test :refer :all]
            [clojureray.ray :as ray]
            [clojureray.comparison :refer :all]))

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
  (testing "A ray intersects a sphere at two points")
  (is (aeq (ray/intersects [0.0 0.0 -5.0 1.0] [0.0 0.0 1.0 0.0]) [4.0 6.0]))

  (testing "A ray intersects a sphere at a tangent")
  (is (aeq (ray/intersects [0.0 1.0 -5.0 1.0] [0.0 0.0 1.0 0.0]) [5.0 5.0]))

  (testing "A ray misses a sphere")
  (is (= (ray/intersects [0.0 2.0 5.0 1.0] [0.0 0.0 1.0 0.0]) nil))

  (testing "A ray originates inside a sphere")
  (is (aeq (ray/intersects [0.0 0.0 0.0 1.0] [0.0 0.0 1.0 0.0]) [-1.0 1.0]))

  (testing "A sphere is behind a ray")
  (is (aeq (ray/intersects [0.0 0.0 5.0 1.0] [0.0 0.0 1.0 0.0]) [-6.0 -4.0]))
  )
