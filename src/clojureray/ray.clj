(ns clojureray.ray
  (:require [clojureray.vector :as vector]
            [clojureray.comparison :refer :all]
            [clojureray.matrix :as matrix]))

(defn ray
  [origin direction]
  {:origin    origin
   :direction direction})

(defn get-origin
  [ray]
  (let [{origin :origin} ray] origin))

(defn get-direction
  [ray]
  (let [{direction :direction} ray] direction))

(defn transform
  [r m]
  (let [{origin    :origin
         direction :direction} r
        new_origin (matrix/multiply-vector m origin)
        new_direction (matrix/multiply-vector m direction)]
    (ray new_origin new_direction)
    )
  )

(defn intersection
  [t object]
  {:t      t
   :object object})

(defn position
  [ray dist]
  (let [{origin    :origin
         direction :direction} ray]
    (vector/add origin (vector/scalar-multiplication dist direction))
    )
  )

(defn normalize
  [ray]
  (let [normalized (vector/normalize (:direction ray))]
    (assoc ray :direction normalized)
    )
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
  (let [ray_transformed (transform ray (matrix/invert (:transformation sphere)))
        {origin    :origin
         direction :direction} ray_transformed]
    (intersects-sphere origin direction sphere)
    )
  )

(defn- get-t
  [i]
  (let [{t :t} i] t))

(defn sort-intersections
  [intersections]
  (sort-by get-t (filter (fn [i] (>= (get-t i) 0)) intersections))
  )

(defn hit
  [intersections]
  (first (sort-intersections intersections)))

(defmulti normal-at (fn [shape _] (:shape shape)))

(defmethod normal-at :sphere [sphere world-point]
  (let [inverse-transform (matrix/invert (:transformation sphere))
        object-point (matrix/multiply-vector inverse-transform world-point)
        object-normal (vector/subtract object-point [0.0 0.0 0.0 1.0])
        world-normal (matrix/multiply-vector (matrix/transpose inverse-transform) object-normal)]
    (vector/normalize (assoc world-normal 3 0.0)))
  )

(defn reflect
  [in normal]
  (let [n-len (vector/dot in normal)
        n-pos (vector/scalar-multiplication (* 2 n-len) normal)]
    (vector/subtract in n-pos)
    )
  )

(defn- compute-diffuse-specular
  [lightv normalv eyev light light-dot-normal effective-color material]
  (if (< light-dot-normal 0)
    {:diffuse  [0.0 0.0 0.0]
     :specular [0.0 0.0 0.0]}
    (let [diffuse (vector/scalar-multiplication light-dot-normal (vector/scalar-multiplication (:diffuse material) effective-color))
          reflectv (reflect (vector/negate lightv) normalv)
          reflect-dot-eye (vector/dot reflectv eyev)
          specular (if (<= reflect-dot-eye 0)
                     [0.0 0.0 0.0]
                     (let [factor (Math/pow reflect-dot-eye (:shininess material))]
                       (vector/scalar-multiplication factor (vector/scalar-multiplication (:specular material) (:intensity light))))
                     )]
      {:diffuse  diffuse
       :specular specular}
      )
    )
  )

(defn lighting
  [material light point eyev normalv in-shadow]
  (let [effective-color (vector/times (:color material) (:intensity light))
        lightv (vector/normalize (vector/subtract (:position light) point))
        ambient (vector/scalar-multiplication (:ambient material) effective-color)
        light-dot-normal (vector/dot lightv normalv)
        diff-spec (compute-diffuse-specular lightv normalv eyev light light-dot-normal effective-color material)
        {diffuse  :diffuse
         specular :specular} diff-spec]
    (if in-shadow [0.1 0.1 0.1]
                  (vector/add (vector/add ambient diffuse) specular))
    )
  )
