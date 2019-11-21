(ns clojureray.vector)

(defn abs
  [n]
  (if (< n 0) (- n) n))

(defn float=
  [f1 f2]
  (< (abs (- f1 f2)) 0.00001))

(defn add
  [v1 v2]
  (mapv + v1 v2)
  )

(defn subtract
  [v1 v2]
  (mapv - v1 v2)
  )

(defn negate
  [v]
  (mapv - v)
  )

(defn scalar-multiplication
  [s v]
  (mapv (fn [e] (* s e)) v)
  )

(defn dot
  [v1 v2]
  (reduce + (mapv * v1 v2))
  )

(defn squared-length
  [v]
  (dot v v)
  )

(defn length
  [v1]
  (Math/sqrt (squared-length v1)))

(defn cross
  [[x1 x2 x3 _] [y1 y2 y3 _]]
  [(- (* x2 y3) (* x3 y2))
   (- (* x3 y1) (* x1 y3))
   (- (* x1 y2) (* x2 y1))
   0])
