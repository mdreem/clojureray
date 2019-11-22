(ns clojureray.transformation)

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
