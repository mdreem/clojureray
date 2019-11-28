(ns clojureray.file
  (:require [clojureray.canvas :as canvas]))

(defn- color-to-string
  [color]
  (let [[r g b] color]
    (format "%4d %4d %4d" r g b)
    )
  )

(defn- concat-colors
  [e1 e2]
  (format "%s  %s" e1 e2)
  )

(defn- concat-rows
  [r1 r2]
  (format "%s\n%s" r1 r2)
  )

(defn convert-color
  [color]
  (mapv (fn [c]
          (int (* 255 (min 1.0 c)))) color)
  )

(defn convert-canvas
  [canvas]
  (reduce concat-rows
          (mapv (fn [row]
                  (reduce concat-colors (mapv (fn [e] (color-to-string (convert-color e))) row))
                  ) canvas)
          ))

(defn ppm-header
  [width height]
  (clojure.string/join "\n" ["P3"
                             (format "%d %d" width height)
                             "255"])
  )

(defn write-file
  [canvas filename]
  (let [header (ppm-header (canvas/get-width canvas) (canvas/get-height canvas))
        converted-canvas (convert-canvas canvas)]
    (with-open [w (clojure.java.io/writer filename)]
      (.write w header)
      (.write w "\n")
      (.write w converted-canvas)
      (.write w "\n")))
  )
