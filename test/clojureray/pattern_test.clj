(ns clojureray.pattern-test
  (:require [clojure.test :refer :all]
            [clojureray.pattern :as pattern]
            [clojureray.util :as util]
            [clojureray.shape :as shape]
            [clojureray.ray :as ray]
            [clojureray.comparison :refer :all]
            [clojureray.matrix :as matrix]
            [clojureray.transformation :as transformation]))

(deftest stripe-patterns
  (let [white (util/color 1 1 1)
        black (util/color 0 0 0)
        stripe (pattern/stripe-pattern white black matrix/id-4)]
    (testing "A stripe pattern is constant in y"
      (is (= (stripe shape/plane (util/point 0 0 0)) white))
      (is (= (stripe shape/plane (util/point 0 1 0)) white))
      (is (= (stripe shape/plane (util/point 0 2 0)) white))
      )

    (testing "A stripe pattern is constant in z"
      (is (= (stripe shape/plane (util/point 0 0 0)) white))
      (is (= (stripe shape/plane (util/point 0 0 1)) white))
      (is (= (stripe shape/plane (util/point 0 0 2)) white))
      )

    (testing "A stripe pattern alternates in x"
      (is (= (stripe shape/plane (util/point 0 0 0)) white))
      (is (= (stripe shape/plane (util/point 0.9 0 0)) white))
      (is (= (stripe shape/plane (util/point 1 0 0)) black))
      (is (= (stripe shape/plane (util/point -0.1 0 0)) black))
      (is (= (stripe shape/plane (util/point -1 0 0)) black))
      (is (= (stripe shape/plane (util/point -1.1 0 0)) white))
      )
    )
  )

(deftest pattern-with-lighting
  (testing "Lighting with a pattern applied"
    (let [white (util/color 1 1 1)
          black (util/color 0 0 0)
          material (-> shape/default-material
                       (shape/set-ambient 1)
                       (shape/set-diffuse 0)
                       (shape/set-specular 0)
                       (shape/set-color (pattern/stripe-pattern white black matrix/id-4)))
          eyev (util/ray-vector 0 0 -1)
          normalv (util/ray-vector 0 0 -1)
          light (shape/point-light (util/point 0 0 -10) (util/color 1 1 1))
          c1 (ray/lighting material shape/plane light (util/point 0.9 0 0) eyev normalv false)
          c2 (ray/lighting material shape/plane light (util/point 1.1 0 0) eyev normalv false)]
      (is (aeq c1 white))
      (is (aeq c2 black))
      )
    )
  )

(deftest stripes-with-transformations
  (let [white (util/color 1 1 1)
        black (util/color 0 0 0)
        t (transformation/scaling 2 2 2)]
    (testing "Stripes with an object transformation"
      (let [pattern (pattern/stripe-pattern white black matrix/id-4)
            object (-> (shape/sphere 1)
                       (shape/set-transformation t))
            color (pattern object (util/point 1.5 0 0))]
        (is (= color white))
        )
      )

    (testing "Stripes with a pattern transformation"
      (let [pattern (pattern/stripe-pattern white black t)
            object (-> (shape/sphere 1)
                       (shape/set-transformation matrix/id-4))
            color (pattern object (util/point 1.5 0 0))]
        (is (= color white))
        )
      )

    (testing "Stripes with a pattern and an object transformation"
      (let [trans (transformation/translation 0.5 0 0)
            pattern (pattern/stripe-pattern white black trans)
            object (-> (shape/sphere 1)
                       (shape/set-transformation t))
            color (pattern object (util/point 2.5 0 0))]
        (is (= color white))
        )
      )
    )
  )

(deftest gradient-patterns
  (let [white (util/color 1 1 1)
        black (util/color 0 0 0)
        gradient (pattern/gradient-pattern white black matrix/id-4)]
    (testing "A stripe pattern is constant in y"
      (is (aeq (gradient shape/plane (util/point 0 0 0)) white))
      (is (aeq (gradient shape/plane (util/point 0.25 0 0)) (util/color 0.75 0.75 0.75)))
      (is (aeq (gradient shape/plane (util/point 0.5 0 0)) (util/color 0.5 0.5 0.5)))
      (is (aeq (gradient shape/plane (util/point 0.75 0 0)) (util/color 0.25 0.25 0.25)))
      )
    )
  )
