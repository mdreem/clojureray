(ns clojureray.example.first-sphere
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

(defn compute-color
  [ray hit light]
  (let [{t      :t
         object :object} hit
        point (ray/position ray t)
        normal (ray/normal-at object point)
        eye (vector/negate (:direction ray))
        color (ray/lighting (:material object) light point eye normal)
        ]
    color
    )
  )

(defn convert-color
  [color]
  (mapv (fn [c]
          (int (* 255 (min 1.0 c)))) color)
  )

(defn compute-ray
  [x y width height wall_width wall_height origin transformed_s light]
  (let [target (compute-pixel-pos width height wall_width wall_height x y 10.0)
        direction (vector/subtract target origin)
        r (ray/normalize (ray/ray origin direction))
        hit (ray/hit (ray/intersect transformed_s r))]
    (if hit (convert-color (compute-color r hit light))
            [0 0 0])
    )
  )

(defn- set-material
  [sphere]
  (let [material shape/default-material
        material-colored (assoc material :color [1.0 0.2 1.0])]
    (assoc sphere :material material-colored)
    )
  )

(defn project-sphere
  [width height]
  (let [sphere (shape/sphere 1.0)
        t_scale (transformation/scaling 0.5 1.0 1.0)
        t_rotate (transformation/rotation_z (/ Math/PI 4))
        t (matrix/multiply t_rotate t_scale)
        colored_s (set-material sphere)
        transformed_s (transformation/set-transform colored_s t)
        origin [0.0 0.0 -5.0 1.0]
        wall_height 7
        wall_width 7
        light-position [-10.0 -10.0 -10.0 1.0]
        light-color [1.0 1.0 1.0]
        light (shape/point-light light-position light-color)]
    (mapv (fn [row]
            (mapv (fn [column]
                    (compute-ray column row width height wall_width wall_height origin transformed_s light))
                  (range width))
            ) (range height))
    )
  )

(defn write-file
  [width height filename]
  (let [res (project-sphere width height)]
    (file/write-file res filename)
    )
  )
