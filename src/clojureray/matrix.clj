(ns clojureray.matrix
  (:require [clojureray.vector :as vector]))

(defn transpose
  [matrix]
  (let [column-count (count (first matrix))]
    (mapv (fn [column-index]
            (mapv (fn [row] (get row column-index)) matrix))
          (range column-count))))

(defn- row-times-matrix
  [row matrix]
  (let [matrix-transposed (transpose matrix)]
    (let [column-count (count row)]
      (mapv (fn [column-index] (vector/dot row (get matrix-transposed column-index)))
            (range column-count))
      )))

(defn multiply
  [matrix1 matrix2]
  (mapv (fn [row] (row-times-matrix row matrix2)) matrix1)
  )
