(ns clojureray.ray
  (:require [clojureray.vector :as vector]
            [clojureray.comparison :refer :all]))

(defn ray
  [origin direction]
  {:origin    origin
   :direction direction})

(defn intersection
  [t object]
  {:t      t
   :object object})

(defn position
  [[origin direction] dist]
  (vector/add origin (vector/scalar-multiplication dist direction))
  )

(defn intersects-sphere
  [origin direction sphere]
  (let [sphere_to_ray (vector/subtract origin [0.0 0.0 0.0 1.0])
        a (vector/dot direction direction)
        b (* 2 (vector/dot direction sphere_to_ray))
        c (- (vector/dot sphere_to_ray sphere_to_ray) 1.0)
        discriminant (- (* b b) (* 4 (* a c)))
        discriminant_root (Math/sqrt discriminant)
        t1 (/ (- (+ b discriminant_root)) (* 2 a))
        t2 (/ (- (- b discriminant_root)) (* 2 a))]
    (if (< discriminant 0) [] [(intersection t1 sphere) (intersection t2 sphere)])
    )
  )

(defmulti intersect (fn [shape _] (:shape shape)))

(defmethod intersect :sphere [sphere ray]
  (let [{origin :origin direction :direction} ray]
    (intersects-sphere origin direction sphere)
    )
  )
