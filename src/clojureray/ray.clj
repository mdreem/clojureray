(ns clojureray.ray
  (:require [clojureray.vector :as vector]))

(defn position
  [[point vector] dist]
  (vector/add point (vector/scalar-multiplication dist vector))
  )
