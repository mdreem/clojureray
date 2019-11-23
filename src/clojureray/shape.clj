(ns clojureray.shape
  (:require [clojureray.matrix :as matrix]))

(defn sphere
  [radius]
  {:shape :sphere :radius radius :transformation matrix/id-4}
  )
