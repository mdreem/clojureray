(ns clojureray.ray
  (:require [clojureray.vector :as vector]
            [clojureray.comparison :refer :all]))

(defn position
  [[origin direction] dist]
  (vector/add origin (vector/scalar-multiplication dist direction))
  )

(defn intersects
  [origin direction]
  (let [sphere_to_ray (vector/subtract origin [0.0 0.0 0.0 1.0])
        a (vector/dot direction direction)
        b (* 2 (vector/dot direction sphere_to_ray))
        c (- (vector/dot sphere_to_ray sphere_to_ray) 1.0)
        discriminant (- (* b b) (* 4 (* a c)))
        discriminant_root (Math/sqrt discriminant)
        t1 (/ (- (+ b discriminant_root)) (* 2 a))
        t2 (/ (- (- b discriminant_root)) (* 2 a))]
    (if (< discriminant 0) nil [t1 t2])
    )
  )
