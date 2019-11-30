(ns clojureray.shape
  (:require [clojureray.matrix :as matrix]
            [clojureray.util :as util]))


(defn create-material
  [color ambient diffuse specular shininess reflective]
  {:color      color
   :ambient    (double ambient)
   :diffuse    (double diffuse)
   :specular   (double specular)
   :shininess  (double shininess)
   :reflective (double reflective)}
  )

(defn set-color
  [material color]
  (assoc material :color color)
  )

(defn set-specular
  [material specular]
  (assoc material :specular (double specular))
  )

(defn set-diffuse
  [material diffuse]
  (assoc material :diffuse (double diffuse))
  )

(defn set-ambient
  [material ambient]
  (assoc material :ambient (double ambient))
  )

(defn set-reflective
  [material reflective]
  (assoc material :reflective (double reflective))
  )

(def default-material
  (create-material (util/constant-color 1 1 1) 0.1 0.9 0.9 200 0)
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
