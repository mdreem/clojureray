(ns clojureray.refraction)

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
  ([intersections hit containers]
   (let [cur-intersection (first intersections)
         object (:object cur-intersection)
         hit-is-intersection (= hit cur-intersection)
         n1 (if hit-is-intersection (compute-n containers) nil)
         updated-containers (update-containers containers object)
         n2 (if hit-is-intersection (compute-n updated-containers) nil)]
     (if hit-is-intersection {:n1 n1
                              :n2 n2}
                             (process-intersections (subvec intersections 1) hit updated-containers))
     ))
  ([intersections hit]
   (process-intersections intersections hit []))
  )