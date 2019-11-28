(ns clojureray.camera
  (:require [clojureray.matrix :as matrix]
            [clojureray.vector :as vector]
            [clojureray.ray :as ray]))

(defn camera
  [hsize vsize field-of-view]
  (let [half-view (Math/tan (/ field-of-view 2))
        aspect (/ hsize vsize)
        half-width (if (> aspect 1) half-view (* half-view aspect))
        half-height (if (> aspect 1) (/ half-view aspect) half-view)
        pixel-size (/ (* 2 half-width) hsize)]
    {:hsize          hsize
     :vsize          vsize
     :field-of-view  field-of-view
     :transformation matrix/id-4
     :half-width     half-width
     :half-height    half-height
     :pixel-size     pixel-size})
  )

(defn ray-for-pixel
  [camera pixel-x pixel-y]
  (let [{pixel-size     :pixel-size
         half-width     :half-width
         half-height    :half-height
         transformation :transformation} camera
        x-offset (* (+ pixel-x 0.5) pixel-size)
        y-offset (* (+ pixel-y 0.5) pixel-size)
        world-x (- half-width x-offset)
        world-y (- half-height y-offset)
        inverse-transform (matrix/invert transformation)
        pixel (matrix/multiply-vector inverse-transform [world-x world-y -1.0 1.0])
        origin (matrix/multiply-vector inverse-transform [0.0 0.0 0.0 1.0])
        direction (vector/normalize (vector/subtract pixel origin))]
    (ray/ray origin direction)
    )
  )
