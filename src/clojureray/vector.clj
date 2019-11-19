(ns clojureray.vector)

(defn abs
  [n]
  (if (< n 0) (- n) n))

(defn float=
  [f1 f2]
  (< (abs (- f1 f2)) 0.00001))

(defn add
  [[x1 x2 x3 w1] [y1 y2 y3 w2]]
  [(+ x1 y1) (+ x2 y2) (+ x3 y3) (+ w1 w2)])

(defn subtract
  [[x1 x2 x3 w1] [y1 y2 y3 w2]]
  [(- x1 y1) (- x2 y2) (- x3 y3) (- w1 w2)])

(defn negate
  [[x1 x2 x3 w1]]
  [(- x1) (- x2) (- x3) (- w1)])

(defn scalar-multiplication
  [s [x1 x2 x3 w1]]
  [(* s x1) (* s x2) (* s x3) (* s w1)])

(defn squared-length
  [[x1 x2 x3 w1]]
  (+ (* x1 x1) (* x2 x2) (* x3 x3) (* w1 w1)))

(defn length
  [v1]
  (Math/sqrt (squared-length v1)))

(defn dot
  [[x1 x2 x3 w1] [y1 y2 y3 w2]]
  (+ (* x1 y1) (* x2 y2) (* x3 y3) (* w1 w2)))

(defn cross
  [[x1 x2 x3 _] [y1 y2 y3 _]]
  [(- (* x2 y3) (* x3 y2))
   (- (* x3 y1) (* x1 y3))
   (- (* x1 y2) (* x2 y1))
   0])
