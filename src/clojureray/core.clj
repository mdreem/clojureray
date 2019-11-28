(ns clojureray.core
  (:gen-class)
  (:require [clojureray.example.first-scene :as first-scene]))

(defn -main
  "Generate a scene."
  [& args]
  ;(first-scene/write-file 400 200 "test.ppm"))
  (first-scene/write-file 100 50 "test.ppm"))
