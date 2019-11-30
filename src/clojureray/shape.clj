(ns clojureray.shape
  (:require [clojureray.matrix :as matrix]
            [clojureray.util :as util]))


(defn create-material
  [color ambient diffuse specular shininess]
  {:color     color
   :ambient   ambient
   :diffuse   diffuse
   :specular  specular
   :shininess shininess
   }
  )

(defn set-color
  [material color]
  (assoc material :color color)
  )

(defn set-specular
  [material specular]
  (assoc material :specular specular)
  )

(defn set-diffuse
  [material diffuse]
  (assoc material :diffuse diffuse)
  )

(def default-material
  (create-material (util/constant-color 1 1 1) 0.1 0.9 0.9 200.0)
  )

(defn set-material
  [shape material]
  (assoc shape :material material)
  )

(defn set-transformation
  [shape transformation]
  (assoc shape :transformation transformation)
  )

(defn sphere
  [radius]
  {:shape          :sphere
   :radius         radius
   :transformation matrix/id-4
   :material       default-material}
  )

(def plane
  {:shape          :plane
   :transformation matrix/id-4
   :material       default-material}
  )

(defn point-light
  [position intensity]
  {:position  position
   :intensity intensity}
  )
