(ns clojureray.transformations)

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
