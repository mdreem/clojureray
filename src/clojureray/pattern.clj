(ns clojureray.pattern
  (:require [clojureray.util :as util]
            [clojureray.matrix :as matrix]))

(defn stripe-pattern
  [color1 color2 t]
  (let [inverse (matrix/invert t)]
    (fn [object p] (let [object-transform (:transformation object)
                         object-space (matrix/multiply-vector (matrix/invert object-transform) p)
                         pattern-space (matrix/multiply-vector inverse object-space)
                         p-x (get pattern-space 0)
                         check (even? (int (Math/floor p-x)))]
                     (if check color1 color2)
                     )
      ))
  )
