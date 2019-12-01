(ns clojureray.refraction
  (:require [clojureray.comparison :refer :all]
            [clojureray.util :as util]
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

(defn process-intersections
  ([[cur-intersection & intersections] hit containers]
   (let [object (:object cur-intersection)
         hit-is-intersection (= hit cur-intersection)
         n1 (if hit-is-intersection (compute-n containers) nil)
         updated-containers (update-containers containers object)
         n2 (if hit-is-intersection (compute-n updated-containers) nil)]
     (if hit-is-intersection {:n1 n1
                              :n2 n2}
                             (process-intersections intersections hit updated-containers))
     ))
  ([intersections hit]
   (process-intersections intersections hit []))
  )

(defn refracted-color
  [world comps remaining]
  (let [{object  :object
         n1      :n1
         n2      :n2
         eyev    :eyev
         normalv :normalv} comps
        transparency (get-in object [:material :transparency])
        n-ratio (/ n1 n2)
        cos-i (vector/dot eyev normalv)
        sin2-t (* (* n-ratio n-ratio) (- 1 (* cos-i cos-i)))]
    (println "sin2-t" sin2-t)
    (if (or (<= remaining 0) (aeq 0.0 transparency) (> sin2-t 1.0))
      (util/color 0 0 0)
      (util/color 1 1 1)
      )
    )
  )
