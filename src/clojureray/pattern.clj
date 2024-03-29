(ns clojureray.pattern
  (:require [clojureray.util :as util]
            [clojureray.matrix :as matrix]
            [clojureray.vector :as vector]))

(defn test-pattern
  [t]
  (let [inverse (matrix/invert t)]
    (fn [object p] (let [object-transform (:transformation object)
                         object-space (matrix/multiply-vector (matrix/invert object-transform) p)
                         pattern-space (matrix/multiply-vector inverse object-space)]
                     (util/color (get pattern-space 0) (get pattern-space 1) (get pattern-space 2))
                     )
      )
    )
  )

(defn stripe-pattern
  [color1 color2 t]
  (let [inverse (matrix/invert t)]
    (fn [object p] (let [object-transform (:transformation object)
                         object-space (matrix/multiply-vector (matrix/invert object-transform) p)
                         pattern-space (matrix/multiply-vector inverse object-space)
                         p-x (get pattern-space 0)
                         check (even? (int (Math/floor p-x)))]
                     (if check color1 color2)
                     )
      )
    )
  )

(defn gradient-pattern
  [color1 color2 t]
  (let [inverse (matrix/invert t)]
    (fn [object p] (let [object-transform (:transformation object)
                         object-space (matrix/multiply-vector (matrix/invert object-transform) p)
                         pattern-space (matrix/multiply-vector inverse object-space)
                         p-x (get pattern-space 0)
                         f-p-x (Math/floor p-x)]
                     (vector/add color1 (vector/scalar-multiplication (- p-x f-p-x)
                                                                      (vector/subtract color2 color1)))
                     )
      )
    )
  )

(defn ring-pattern
  [color1 color2 t]
  (let [inverse (matrix/invert t)]
    (fn [object p] (let [object-transform (:transformation object)
                         object-space (matrix/multiply-vector (matrix/invert object-transform) p)
                         pattern-space (matrix/multiply-vector inverse object-space)
                         p-x (get pattern-space 0)
                         p-z (get pattern-space 2)
                         d (int (Math/floor (Math/sqrt (+ (* p-x p-x) (* p-z p-z)))))]
                     (if (even? d) color1 color2)
                     )
      )
    )
  )

(defn checker-pattern
  [color1 color2 t]
  (let [inverse (matrix/invert t)]
    (fn [object p] (let [object-transform (:transformation object)
                         object-space (matrix/multiply-vector (matrix/invert object-transform) p)
                         pattern-space (matrix/multiply-vector inverse object-space)
                         p-x (int (Math/floor (get pattern-space 0)))
                         p-y (int (Math/floor (get pattern-space 1)))
                         p-z (int (Math/floor (get pattern-space 2)))]
                     (if (even? (+ p-x p-y p-z)) color1 color2)
                     )
      )
    )
  )

