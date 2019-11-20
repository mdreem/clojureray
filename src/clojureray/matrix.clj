(ns clojureray.matrix
  (:require [clojureray.vector :as vector]))

(def id-4
  [[1 0 0 0]
   [0 1 0 0]
   [0 0 1 0]
   [0 0 0 1]])

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

(defn- drop-element
  [coll pos]
  (concat (subvec coll 0 pos) (subvec coll (inc pos)))
  )

(defn submatrix
  [matrix, row, column]
  (let [row-removed (drop-element matrix row)]
    (mapv (fn [row] (drop-element row column)) row-removed)
    )
  )
