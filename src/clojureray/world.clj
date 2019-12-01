(ns clojureray.world
  (:require [clojureray.comparison :refer :all]
            [clojureray.ray :as ray]
            [clojureray.shape :as shape]
            [clojureray.transformation :as transformation]
            [clojureray.vector :as vector]
            [clojureray.refraction :as refraction]
            [clojureray.util :as util]))

(def empty-world
  {:shapes []
   :lights []})

(defn add-shape
  [world shape]
  (let [shapes (:shapes world)
        shapes-new (doall (conj shapes shape))]
    (assoc world :shapes shapes-new)
    ))

(defn add-light
  [world light]
  (let [lights (:lights world)
        lights-new (doall (conj lights light))]
    (assoc world :lights lights-new)
    ))

(def default-world
  (let [material (shape/create-material (util/constant-color 0.8 1.0 0.6) 0.1 0.7 0.2 200 0 1)
        sphere-1 (-> (shape/sphere 1)
                     (shape/set-material material))
        sphere-2 (-> (shape/sphere 1)
                     (shape/set-material shape/default-material)
                     (transformation/set-transform (transformation/scaling 0.5 0.5 0.5)))
        light (shape/point-light [-10.0 10.0 -10.0 1.0] [1.0 1.0 1.0])
        world (-> empty-world
                  (add-shape sphere-1)
                  (add-shape sphere-2)
                  (add-light light))
        ]
    world)
  )

(defn intersect-world
  [world ray]
  (let [{shapes :shapes
         lights :lights} world]
    (ray/sort-intersections
      (flatten
        (mapv (fn [shape] (ray/intersect shape ray)) shapes))
      )
    )
  )

(defn prepare-computations
  [intersection ray xs]
  (let [{t      :t
         object :object} intersection
        {direction :direction} ray
        position (ray/position ray t)
        eyev (vector/negate direction)
        normalv (ray/normal-at object position)
        inside (< (vector/dot normalv eyev) 0)
        over-point (vector/add position (vector/scalar-multiplication util/epsilon normalv))
        under-point (vector/subtract position (vector/scalar-multiplication util/epsilon normalv))
        reflectv (ray/reflect direction normalv)
        {n1 :n1
         n2 :n2} (refraction/process-intersections xs intersection)]
    {:t           t
     :object      object
     :point       position
     :eyev        eyev
     :normalv     (if inside (vector/negate normalv) normalv)
     :inside      inside
     :over-point  over-point
     :under-point under-point
     :reflectv    reflectv
     :n1          n1
     :n2          n2}
    )
  )

(defn is-shadowed-with-light-
  [world light point]
  (let [v (vector/subtract (:position light) point)
        distance (vector/length v)
        direction (vector/normalize v)
        r (ray/ray point direction)
        intersections (intersect-world world r)
        h (ray/hit intersections)]
    (if h (< (:t h) distance)
          false)
    )
  )

(defn is-shadowed
  [world point]
  (let [lights (:lights world)
        s (mapv (fn [light] (is-shadowed-with-light- world light point)) lights)]
    (every? true? s)
    )
  )

(defn- add-colors
  ([v1 v2] (vector/add v1 v2))
  ([v] v)
  ([] nil)
  )

(declare reflected-color)

(defn shade-hit
  [world comps remaining]
  (let [{object     :object
         point      :point
         eyev       :eyev
         normalv    :normalv
         over-point :over-point} comps
        material (:material object)
        lights (:lights world)
        surface-color (reduce add-colors
                              (mapv (fn [light] (ray/lighting material object light point eyev normalv (is-shadowed world over-point)))
                                    lights)
                              )
        reflected (reflected-color world comps remaining)]
    (vector/add surface-color reflected)
    )
  )

(defn color-at
  [ray world remaining]
  (let [intersections (intersect-world world ray)
        hit (first intersections)]
    (if hit (let [computations (prepare-computations hit ray [hit])
                  shade (shade-hit world computations remaining)] shade)
            (util/color 0 0 0)
            )
    )
  )

(defn reflected-color
  [world comps remaining]
  (let [reflective (get-in comps [:object :material :reflective])]
    (if (<= remaining 0) (util/color 0 0 0)
                         (if (aeq reflective 0) (util/color 0 0 0)
                                                (let [{over-point :over-point
                                                       reflectv   :reflectv} comps
                                                      reflect-ray (ray/ray over-point reflectv)
                                                      color (color-at reflect-ray world (dec remaining))]
                                                  (vector/scalar-multiplication reflective color)
                                                  )
                                                )
                         )
    )
  )
