(ns clojureray.canvas)

(defn create-canvas
  [width height]
  (vec (repeat height (vec (repeat width [0 0 0]))))
  )

(defn set-pixel
  [canvas x y color]
  (assoc-in canvas [y x] color)
  )

(defn get-pixel
  [canvas x y]
  (get-in canvas [y x])
  )

(defn get-width
  [canvas]
  (count (first canvas)))

(defn get-height
  [canvas]
  (count canvas))
