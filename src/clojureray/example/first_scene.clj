(ns clojureray.example.first-scene
  (:require [clojureray.shape :as shape]
            [clojureray.transformation :as transformation]
            [clojureray.file :as file]
            [clojureray.world :as world]
            [clojureray.camera :as camera]
            [clojureray.matrix :as matrix]))

(def light (shape/point-light [-10.0 10.0 -10.0 1.0] [0.2 0.5 0.2]))
(def light-sec (shape/point-light [10.0 10.0 -5.0 1.0] [0.2 0.2 0.2]))

(def floor-material
  (-> shape/default-material
      (shape/set-color [1.0 0.9 0.9])
      (shape/set-specular 0.0))
  )

(def floor
  (-> (shape/sphere 1)
      (shape/set-transformation (transformation/scaling 10.0 0.01 10.0))
      (shape/set-material floor-material)
      )
  )

(def left-wall
  (let [t (transformation/translation 0.0 0.0 5.0)
        r_y (transformation/rotation_y (- (/ Math/PI 4)))
        r_x (transformation/rotation_x (/ Math/PI 2))
        s (transformation/scaling 10.0 0.01 10.0)
        transform (->> s
                       (matrix/multiply r_x)
                       (matrix/multiply r_y)
                       (matrix/multiply t))
        ]
    (-> (shape/sphere 1)
        (shape/set-transformation transform)
        (shape/set-material floor-material))
    )
  )

(def right-wall
  (let [t (transformation/translation 0.0 0.0 5.0)
        r_y (transformation/rotation_y (/ Math/PI 4))
        r_x (transformation/rotation_x (/ Math/PI 2))
        s (transformation/scaling 10.0 0.01 10.0)
        transform (->> s
                       (matrix/multiply r_x)
                       (matrix/multiply r_y)
                       (matrix/multiply t))
        ]
    (-> (shape/sphere 1)
        (shape/set-transformation transform)
        (shape/set-material floor-material))
    )
  )

(def middle
  (-> (shape/sphere 1)
      (shape/set-transformation (transformation/translation -0.5 1.0 0.5))
      (shape/set-material (-> shape/default-material
                              (shape/set-color [0.1 1.0 0.5])
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
                                (shape/set-color [0.5 1.0 0.1])
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
                                (shape/set-color [1.0 0.8 0.1])
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
      (world/add-shape left-wall)
      (world/add-shape right-wall)
      (world/add-shape middle)
      (world/add-shape right)
      (world/add-shape left)
      (world/add-light light)
      (world/add-light light-sec)
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
