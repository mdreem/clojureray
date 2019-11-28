(ns clojureray.matrix
  (:require [clojureray.vector :as vector]))

(def id-4
  [[1.0 0.0 0.0 0.0]
   [0.0 1.0 0.0 0.0]
   [0.0 0.0 1.0 0.0]
   [0.0 0.0 0.0 1.0]])

(defn transpose
  [matrix]
  (let [column-count (count (first matrix))]
    (mapv (fn [column-index]
            (mapv (fn [row] (get row column-index)) matrix))
          (range column-count))))

(defn- row-times-matrix
  [row matrix]
  (let [matrix-transposed (transpose matrix)]
    (let [column-count (count matrix-transposed)]
      (mapv (fn [column-index] (vector/dot row (get matrix-transposed column-index)))
            (range column-count))
      )))

(defn multiply
  [matrix1 matrix2]
  (mapv (fn [row] (row-times-matrix row matrix2)) matrix1)
  )

(defn multiply-vector
  [matrix v]
  (first (transpose (multiply matrix (transpose [v]))))
  )

(defn- drop-element
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos))))
  )

(defn submatrix
  [matrix row column]
  (let [row-removed (drop-element matrix row)]
    (mapv (fn [row] (drop-element row column)) row-removed)
    )
  )

(defn- determinant-2
  [m]
  (- (* (get (get m 0) 0) (get (get m 1) 1)) (* (get (get m 1) 0) (get (get m 0) 1)))
  )

(declare determinant)

(defn minor
  [matrix row column]
  (let [sub (submatrix matrix row column)]
    (let [len (count (first sub))]
      (if (= len 2) (determinant-2 sub) (determinant sub))
      )))

(defn cofactor
  [matrix row column]
  (let [sign (if (even? (+ row column)) 1 -1)]
    (* sign (minor matrix row column)))
  )

(defn determinant
  [matrix]
  (let [len (count matrix)]
    (reduce + (mapv (fn [row] (* (first (get matrix row)) (cofactor matrix row 0))) (range len)))
    )
  )

(defn invert
  [matrix]
  (let [size (count matrix)]
    (let [det (/ (determinant matrix) 1.0)]
      (mapv (fn [row]
              (mapv (fn [column] (/ (cofactor matrix column row) det)) (range size))
              ) (range size))
      )
    )
  )
