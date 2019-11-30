(ns clojureray.example.scene-with-planes
  (:require [clojureray.shape :as shape]
            [clojureray.transformation :as transformation]
            [clojureray.file :as file]
            [clojureray.world :as world]
            [clojureray.camera :as camera]
            [clojureray.matrix :as matrix]
            [clojureray.pattern :as pattern]
            [clojureray.util :as util]))

(def light (shape/point-light [-10.0 10.0 -10.0 1.0] [1.0 1.0 1.0]))

(def floor-material
  (-> shape/default-material
      (shape/set-color (pattern/stripe-pattern
                         (util/color 1.0 0.9 0.9)
                         (util/color 0.1 0.1 0.9)
                         (transformation/rotation_y (/ (Math/PI) 4)))
                       )
      (shape/set-specular 0.0))
  )

(def floor
  (-> shape/plane
      (shape/set-material floor-material)
      )
  )

(def middle
  (-> (shape/sphere 1)
      (shape/set-transformation (transformation/translation -0.5 1.0 0.5))
      (shape/set-material (-> shape/default-material
                              (shape/set-color (util/constant-color 0.5 1.0 0.1))
                              (shape/set-specular 0.3)
                              (shape/set-diffuse 0.7)
                              )
                          )
      )
  )

(def right
  (let [t (transformation/translation 1.5 0.5 -0.5)
        s (transformation/scaling 0.5 0.5 0.5)
        transform (->> s
                       (matrix/multiply t))]
    (-> (shape/sphere 1)
        (shape/set-transformation transform)
        (shape/set-material (-> shape/default-material
                                (shape/set-color (util/constant-color 0.5 1.0 0.1))
                                (shape/set-specular 0.3)
                                (shape/set-diffuse 0.7)
                                )
                            )
        )
    )
  )

(def left
  (let [t (transformation/translation -1.5 0.33 -0.75)
        s (transformation/scaling 0.33 0.33 0.33)
        transform (->> s
                       (matrix/multiply t))]
    (-> (shape/sphere 1)
        (shape/set-transformation transform)
        (shape/set-material (-> shape/default-material
                                (shape/set-color (util/constant-color 1.0 0.8 0.1))
                                (shape/set-specular 0.3)
                                (shape/set-diffuse 0.7)
                                )
                            )
        )
    )
  )

(def world
  (-> world/empty-world
      (world/add-shape floor)
      (world/add-shape middle)
      (world/add-shape right)
      (world/add-shape left)
      (world/add-light light)
      )
  )


(defn camera
  [width height]
  (-> (camera/camera width height (/ Math/PI 3))
      (camera/set-transform (transformation/view-transform [0.0 1.5 -5.0 1.0] [0.0 1.0 0.0 1.0] [0.0 1.0 0.0 0.0]))
      )
  )

(defn write-file
  [width height filename]
  (let [res (camera/render (camera width height) world)]
    (file/write-file res filename)
    )
  )
