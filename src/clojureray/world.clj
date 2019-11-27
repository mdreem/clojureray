(ns clojureray.world
  (:require [clojureray.ray :as ray]
            [clojureray.shape :as shape]
            [clojureray.transformation :as transformation]
            [clojureray.vector :as vector]))

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
  (let [material (shape/create-material [0.8 1.0 0.6] 0.1 0.7 0.2 200.0)
        sphere-1 (-> (shape/sphere 1))
        sphere-2 (-> (shape/sphere 1)
                     (shape/set-material shape/default-material)
                     (transformation/set-transform (transformation/scaling 0.5 0.5 0.5)))
        world (-> empty-world
                  (add-shape sphere-1)
                  (add-shape sphere-2))
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
        ]
    {:t       t
     :object  object
     :point   position
     :eyev    eyev
     :normalv (if inside (vector/negate normalv) normalv)
     :inside  inside}
    )
  )