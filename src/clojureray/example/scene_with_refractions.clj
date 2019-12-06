(ns clojureray.example.scene-with-refractions
  (:require [clojureray.shape :as shape]
            [clojureray.transformation :as transformation]
            [clojureray.file :as file]
            [clojureray.world :as world]
            [clojureray.camera :as camera]
            [clojureray.matrix :as matrix]
            [clojureray.pattern :as pattern]
            [clojureray.util :as util]))

(def light (shape/point-light (util/point 5 20 5) (util/color 1 1 1)))

(def checker-pattern (pattern/checker-pattern
                       (util/color 1 1 1)
                       (util/color 0 0 0)
                       matrix/id-4))


(def floor-material
  (-> shape/default-material
      (shape/set-color checker-pattern)
      (shape/set-specular 0))
  )

(def floor
  (-> shape/plane
      (shape/set-material floor-material)
      (shape/set-transformation (transformation/translation 0 -10 0))
      )
  )

(def sphere-1
  (-> (shape/sphere 1)
      (shape/set-transformation (transformation/scaling 2 2 2))
      (shape/set-material (-> shape/default-material
                              (shape/set-color (util/constant-color 0 0 0))
                              (shape/set-specular 0.3)
                              (shape/set-diffuse 0.7)
                              (shape/set-transparency 0.8)
                              (shape/set-refractive-index 1.5)
                              )
                          )
      )
  )

(def sphere-2
  (-> (shape/sphere 1)
      (shape/set-transformation (transformation/scaling 1 1 1))
      (shape/set-material (-> shape/default-material
                              (shape/set-color (util/constant-color 0 0 0))
                              (shape/set-specular 0.5)
                              (shape/set-diffuse 0.7)
                              (shape/set-transparency 1)
                              (shape/set-refractive-index 1.5)
                              )
                          )
      )
  )

(def world
  (-> world/empty-world
      (world/add-shape floor)
      (world/add-shape sphere-1)
      (world/add-shape sphere-2)
      (world/add-light light)
      )
  )


(defn camera
  [width height]
  (-> (camera/camera width height (/ Math/PI 3))
      (camera/set-transform (transformation/view-transform (util/point 0 8 0) (util/point 0 0 0) (util/point 0 0 1)))
      )
  )

(defn write-file
  [width height filename]
  (let [res (camera/render (camera width height) world)]
    (file/write-file res filename)
    )
  )
