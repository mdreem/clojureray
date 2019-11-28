(ns clojureray.camera-test
  (:require [clojure.test :refer :all]
            [clojureray.comparison :refer :all]
            [clojureray.camera :as camera]
            [clojureray.transformation :as transformation]
            [clojureray.matrix :as matrix]
            [clojureray.world :as world]))

(deftest ray-for-pixel-with-camera
  (let [c (camera/camera 201 101 (/ Math/PI 2))]
    (testing "Constructing a ray through the center of the canvas"
      (let [ray-pixel (camera/ray-for-pixel c 100 50)
            {direction :direction
             origin    :origin} ray-pixel]
        (is (aeq origin [0.0 0.0 0.0 1.0]))
        (is (aeq direction [0.0 0.0 -1.0 0.0]))
        )
      )

    (testing "Constructing a ray through a corner of the canvas"
      (let [ray-pixel (camera/ray-for-pixel c 0 0)
            {direction :direction
             origin    :origin} ray-pixel]
        (is (aeq origin [0.0 0.0 0.0 1.0]))
        (is (aeq direction [0.66519 0.33259 -0.66851 0.0]))
        )
      )

    (testing "Constructing a ray when the camera is transformed"
      (let [rotation (transformation/rotation_y (/ Math/PI 4))
            translation (transformation/translation 0.0 -2.0 5.0)
            transformation (matrix/multiply rotation translation)
            transformed-camera (camera/set-transform c transformation)
            ray-pixel (camera/ray-for-pixel transformed-camera 100 50)
            {direction :direction
             origin    :origin} ray-pixel
            p (Math/sqrt 2)]
        (is (aeq origin [0.0 2.0 -5.0 1.0]))
        (is (aeq direction [(/ p 2) 0.0 (- (/ p 2)) 0.0]))
        )
      )
    )
  )

(deftest render-scene
  (testing "Rendering a world with a camera"
    (let [w world/default-world
          c (camera/camera 11 11 (/ Math/PI 2))
          view-transform (transformation/view-transform [0.0 0.0 -5.0 1.0] [0.0 0.0 0.0 1.0] [0.0 1.0 0.0 0.0])
          transformed-camera (camera/set-transform c view-transform)
          image (camera/render transformed-camera w)]
      (is (aeq (get-in image [5 5]) [0.38066 0.47583 0.2855]))
      )
    )
  )
