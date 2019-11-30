(ns clojureray.util)

(def epsilon 0.00001)

(defn ray-vector
  [x y z]
  [(double x) (double y) (double z) 0.0])

(defn point
  [x y z]
  [(double x) (double y) (double z) 1.0])

(defn color
  [r g b]
  [(double r) (double g) (double b)])

(defn constant-color
  [r g b]
  (fn [_ _] (color r g b))
  )
