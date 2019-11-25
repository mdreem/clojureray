(ns clojureray.example.project-sphere
  (:require [clojureray.shape :as shape]
            [clojureray.vector :as vector]
            [clojureray.transformation :as transformation]
            [clojureray.file :as file]
            [clojureray.ray :as ray]
            [clojureray.matrix :as matrix]))

(defn- compute-pixel-pos
  [width height wall_width wall_height x y wall_pos]
  (let [pos_x (- (* wall_width (/ x width)) (/ wall_width 2.0))
        pos_y (- (* wall_height (/ y height)) (/ wall_height 2.0))]
    [pos_x pos_y wall_pos 1.0]
    )
  )

(defn compute-ray
  [x y width height]
  (let [sphere (shape/sphere 1.0)
        t_scale (transformation/scaling 0.5 1.0 1.0)
        t_rotate (transformation/rotation_z (/ Math/PI 4))
        t (matrix/multiply t_rotate t_scale)
        transformed_s (transformation/set-transform sphere t)
        origin [0.0 0.0 -5.0 1.0]
        wall_height 7
        wall_width 7
        target (compute-pixel-pos width height wall_width wall_height x y 10.0)
        direction (vector/normalize (vector/subtract target origin))
        r (ray/ray origin direction)
        hit (ray/hit (ray/intersect transformed_s r))
        ]
    (if hit [255 0 0] [0 0 0])
    )
  )

(defn project-sphere
  [width height]
  (mapv (fn [row]
          (mapv (fn [column]
                  (compute-ray column row width height))
                (range width))
          ) (range height))
  )

(defn write-file
  [width height filename]
  (let [res (project-sphere width height)]
    (file/write-file res filename)
    )
  )
