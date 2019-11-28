(ns clojureray.transformation
  (:require [clojureray.matrix :as matrix]
            [clojureray.vector :as vector]))

(defn translation
  [x y z]
  [[1.0 0.0 0.0 x]
   [0.0 1.0 0.0 y]
   [0.0 0.0 1.0 z]
   [0.0 0.0 0.0 1.0]])

(defn scaling
  [x y z]
  [[x 0.0 0.0 0]
   [0.0 y 0.0 0]
   [0.0 0.0 z 0]
   [0.0 0.0 0.0 1.0]])

(defn rotation_x
  [rad]
  [[1.0 0.0 0.0 0]
   [0.0 (Math/cos rad) (- (Math/sin rad)) 0.0]
   [0.0 (Math/sin rad) (Math/cos rad) 0.0]
   [0.0 0.0 0.0 1.0]])

(defn rotation_y
  [rad]
  [[(Math/cos rad) 0.0 (Math/sin rad) 0.0]
   [0.0 1.0 0.0 0]
   [(- (Math/sin rad)) 0.0 (Math/cos rad) 0]
   [0.0 0.0 0.0 1.0]])

(defn rotation_z
  [rad]
  [[(Math/cos rad) (- (Math/sin rad)) 0.0 0.0]
   [(Math/sin rad) (Math/cos rad) 0.0 0.0]
   [0.0 0.0 1.0 0]
   [0.0 0.0 0.0 1.0]])

(defn shearing
  [x_y x_x y_x y_z z_x z_y]
  [[1.0 x_y x_x 0.0]
   [y_x 1.0 y_z 0.0]
   [z_x z_y 1.0 0.0]
   [0.0 0.0 0.0 1.0]])

(defn set-transform
  [shape transformation]
  (assoc shape :transformation transformation)
  )

(defn view-transform
  [from to up]
  (let [forward (vector/normalize (vector/subtract to from))
        upn (vector/normalize up)
        left (vector/cross forward upn)
        true_up (vector/cross left forward)
        orientation [[(get left 0) (get left 1) (get left 2) 0.0]
                     [(get true_up 0) (get true_up 1) (get true_up 2) 0.0]
                     [(- (get forward 0)) (- (get forward 1)) (- (get forward 2)) 0.0]
                     [0.0 0.0 0.0 1.0]]
        translation (translation (- (get from 0)) (- (get from 1)) (- (get from 2)))]
    (matrix/multiply orientation translation)
    )
  )
