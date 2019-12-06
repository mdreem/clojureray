(ns clojureray.refraction
  (:require [clojureray.comparison :refer :all]
            [clojureray.vector :as vector]))

(defn at
  [coll e]
  (first (keep-indexed (fn [i x] (when (= x e) i))
                       coll))
  )

(defn remove-at
  [v n]
  (into (subvec v 0 n) (subvec v (inc n)))
  )

(defn update-containers
  [containers object]
  (let [pos (at containers object)]
    (if pos
      (remove-at containers pos)
      (conj containers object)
      )
    )
  )

(defn compute-n
  [containers]
  (if (empty? containers)
    1.0
    (get-in (last containers) [:material :refractive-index])
    )
  )

(defn process-intersections-containers-
  [[cur-intersection & intersections] hit containers]
  (if cur-intersection
    (let [object (:object cur-intersection)
          hit-is-intersection (= hit cur-intersection)
          n1 (if hit-is-intersection (compute-n containers) nil)
          updated-containers (update-containers containers object)
          n2 (if hit-is-intersection (compute-n updated-containers) nil)]
      (if hit-is-intersection {:n1 n1
                               :n2 n2}
                              (process-intersections-containers- intersections hit updated-containers))
      )
    {:n1 1.0
     :n2 1.0})
  )

(defn process-intersections
  [intersections hit]
  (process-intersections-containers- intersections hit [])
  )

(defn- schlick-r
  [n1 n2 cos]
  (let [r0 (* (/ (- n1 n2) (+ n1 n2)) (/ (- n1 n2) (+ n1 n2)))
        cos-pow (Math/pow (- 1 cos) 5)]
    (+ r0 (* (- 1.0 r0) cos-pow))
    )
  )

(defn schlick
  [comps]
  (let [{eyev    :eyev
         normalv :normalv
         n1      :n1
         n2      :n2} comps
        c (vector/dot eyev normalv)]
    (if (> n1 n2)
      (let [n (/ n1 n2)
            sin2-t (* (* n n) (- 1 (* c c)))]
        (if (> sin2-t 1.0)
          1.0
          (schlick-r n1 n2 (Math/sqrt (- 1.0 sin2-t)))
          )
        )
      (schlick-r n1 n2 c)
      )
    )
  )
