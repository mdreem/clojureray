(ns clojureray.patterm-test
  (:require [clojure.test :refer :all]
            [clojureray.pattern :as pattern]
            [clojureray.util :as util]))

(deftest stripe-patterns

  (let [white (util/color 1 1 1)
        black (util/color 0 0 0)
        stripe (pattern/stripe-pattern white black)]
    (testing "A stripe pattern is constant in y"
      (is (= (stripe (util/point 0 0 0)) white))
      (is (= (stripe (util/point 0 1 0)) white))
      (is (= (stripe (util/point 0 2 0)) white))
      )

    (testing "A stripe pattern is constant in z"
      (is (= (stripe (util/point 0 0 0)) white))
      (is (= (stripe (util/point 0 0 1)) white))
      (is (= (stripe (util/point 0 0 2)) white))
      )

    (testing "A stripe pattern alternates in x"
      (is (= (stripe (util/point 0 0 0)) white))
      (is (= (stripe (util/point 0.9 0 0)) white))
      (is (= (stripe (util/point 1 0 0)) black))
      (is (= (stripe (util/point -0.1 0 0)) black))
      (is (= (stripe (util/point -1 0 0)) black))
      (is (= (stripe (util/point -1.1 0 0)) white))
      )
    )
  )
