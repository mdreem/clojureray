(ns clojureray.core
  (:gen-class)
  (:require [clojureray.example.scene-with-refractions :as scene]))

(defn -main
  "Generate a scene."
  [& args]
  ;(first-scene/write-file 400 200 "test.ppm"))
  (scene/write-file 100 50 "test.ppm"))
