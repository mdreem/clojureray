(ns clojureray.shape
  (:require [clojureray.matrix :as matrix]))


(defn create-material
  [color ambient diffuse specular shininess]
  {:color     color
   :ambient   ambient
   :diffuse   diffuse
   :specular  specular
   :shininess shininess
   }
  )

(def default-material
  (create-material [1.0 1.0 1.0] 0.1 0.9 0.9 200.0)
  )

(defn set-material
  [shape material]
  (assoc shape :material material)
  )

(defn sphere
  [radius]
  {:shape          :sphere
   :radius         radius
   :transformation matrix/id-4
   :material       default-material}
  )

(defn point-light
  [position intensity]
  {:position  position
   :intensity intensity}
  )
