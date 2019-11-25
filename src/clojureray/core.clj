(ns clojureray.core
  (:gen-class)
  (:require [clojureray.example.first-sphere :as project-sphere]))

(defn -main
  "Generate a scene."
  [& args]
  (project-sphere/write-file 100 100 "test.ppm"))
