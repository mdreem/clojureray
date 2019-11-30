(ns clojureray.world
  (:require [clojureray.ray :as ray]
            [clojureray.shape :as shape]
            [clojureray.transformation :as transformation]
            [clojureray.vector :as vector]
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
  (let [material (shape/create-material (util/constant-color 0.8 1.0 0.6) 0.1 0.7 0.2 200.0)
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
  [intersection ray]
  (let [{t      :t
         object :object} intersection
        position (ray/position ray t)
        eyev (vector/negate (:direction ray))
        normalv (ray/normal-at object position)
        inside (< (vector/dot normalv eyev) 0)
        over-point (vector/add position (vector/scalar-multiplication util/epsilon normalv))]
    {:t          t
     :object     object
     :point      position
     :eyev       eyev
     :normalv    (if inside (vector/negate normalv) normalv)
     :inside     inside
     :over-point over-point}
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

(defn shade-hit
  [world comps]
  (let [{object     :object
         point      :point
         eyev       :eyev
         normalv    :normalv
         over-point :over-point} comps
        material (:material object)
        lights (:lights world)]
    (reduce add-colors
            (mapv (fn [light] (ray/lighting material light point eyev normalv (is-shadowed world over-point))) lights)
            )
    )
  )

(defn color-at
  [ray world]
  (let [intersections (intersect-world world ray)
        hit (first intersections)]
    (if hit (let [computations (prepare-computations hit ray)
                  shade (shade-hit world computations)] shade) (util/color 0 0 0))
    )
  )
