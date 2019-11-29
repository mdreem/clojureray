(ns clojureray.util)

(def epsilon 0.00001)

(defn ray-vector
  [x y z]
  [(double x) (double y) (double z) 0.0])

(defn point
  [x y z]
  [(double x) (double y) (double z) 1.0])
