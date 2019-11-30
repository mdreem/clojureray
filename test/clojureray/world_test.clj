(ns clojureray.world-test
  (:require [clojure.test :refer :all]
            [clojureray.comparison :refer :all]
            [clojureray.world :as world]
            [clojureray.shape :as shape]
            [clojureray.ray :as ray]
            [clojureray.transformation :as transformation]
            [clojureray.util :as util]))

(deftest tests-helpers

  (testing "Test add shape"
    (let [shape (shape/sphere 1)
          world-with-shape (world/add-shape world/empty-world shape)]
      (is (= (:shapes world-with-shape) [shape]))
      (is (= (:lights world-with-shape) []))
      )
    )

  (testing "Test add three shapes"
    (let [sphere-1 (shape/sphere 1)
          sphere-2 (shape/sphere 2)
          sphere-3 (shape/sphere 3)
          world-with-shape (-> world/empty-world
                               (world/add-shape sphere-1)
                               (world/add-shape sphere-2)
                               (world/add-shape sphere-3))]
      (is (= (:shapes world-with-shape) [sphere-1 sphere-2 sphere-3]))
      (is (= (:lights world-with-shape) []))
      )
    )
  )


(deftest intersect-world
  (testing "Test ray intersecting two spheres"
    (let [ray (ray/ray [0.0 0.0 -5.0 1.0] [0.0 0.0 1.0 0.0])
          intersections (world/intersect-world world/default-world ray)
          t-values (mapv (fn [intersection] (:t intersection)) intersections)]
      (is (= t-values [4.0 4.5 5.5 6.0]))
      )
    )
  )

(deftest prepare-computations-of-intersections
  (let [shape (shape/sphere 1)]
    (testing "Test prepare computations when intersection occurs on the outside")
    (let [r (ray/ray [0.0 0.0 -5.0 1.0] [0.0 0.0 1.0 0.0])
          intersection (ray/intersection 4.0 shape)
          computations (world/prepare-computations intersection r)
          {t       :t
           object  :object
           point   :point
           eyev    :eyev
           normalv :normalv
           inside  :inside} computations
          ]
      (is (= t 4.0))
      (is (= object shape))
      (is (= point [0.0 0.0 -1.0 1.0]))
      (is (= eyev [0.0 0.0 -1.0 0.0]))
      (is (= normalv [0.0 0.0 -1.0 0.0]))
      (is (= inside false)))

    (testing "Test prepare computations when intersection occurs on the inside")
    (let [r (ray/ray [0.0 0.0 0.0 1.0] [0.0 0.0 1.0 0.0])
          intersection (ray/intersection 1.0 shape)
          computations (world/prepare-computations intersection r)
          {t       :t
           object  :object
           point   :point
           eyev    :eyev
           normalv :normalv
           inside  :inside} computations
          ]
      (is (= t 1.0))
      (is (= object shape))
      (is (= point [0.0 0.0 1.0 1.0]))
      (is (= eyev [0.0 0.0 -1.0 0.0]))
      (is (= normalv [0.0 0.0 -1.0 0.0]))
      (is (= inside true)))
    )
  )

(deftest shading-an-intersection
  (testing "Shading an intersection"
    (let [r (ray/ray [0.0 0.0 -5.0 1.0] [0.0 0.0 1.0 0.0])
          intersection (ray/intersection 4.0 (first (:shapes world/default-world)))
          comps (world/prepare-computations intersection r)
          shade-hit (world/shade-hit world/default-world comps)]
      (is (aeq shade-hit [0.38066 0.47583 0.2855]))
      )
    )

  (testing "Shading an intersection from the inside"
    (let [r (ray/ray [0.0 0.0 0.0 1.0] [0.0 0.0 1.0 0.0])
          light (shape/point-light [0.0 0.25 0.0 1.0] [1.0 1.0 1.0])
          intersection (ray/intersection 0.5 (second (:shapes world/default-world)))
          comps (world/prepare-computations intersection r)
          world-light (assoc world/default-world :lights [light])
          shade-hit (world/shade-hit world-light comps)]
      (is (aeq shade-hit [0.1 0.1 0.1]))
      )
    )

  (testing "Shading an intersection in shadow"
    (let [s1 (shape/sphere 1)
          s2 (-> (shape/sphere 1)
                 (shape/set-transformation (transformation/translation 0.0 0.0 10.0))
                 )
          light (shape/point-light (util/point 0 0 -10) [1.0 1.0 1.0])
          w (-> world/empty-world
                (world/add-shape s1)
                (world/add-shape s2)
                (world/add-light light))
          r (ray/ray (util/point 0 0 5) (util/ray-vector 0 0 1))
          intersection (ray/intersection 4.0 s2)
          comps (world/prepare-computations intersection r)
          shade-hit (world/shade-hit w comps)]
      (is (aeq shade-hit [0.1 0.1 0.1]))
      )
    )
  )

(deftest get-color-at
  (testing "The color when a ray misses"
    (let [ray (ray/ray [0.0 0.0 -5.0 1.0] [0.0 1.0 0.0 0.0])
          color (world/color-at ray world/default-world)]
      (is (= color [0.0 0.0 0.0]))
      )
    )

  (testing "The color with an intersection behind the ray"
    (let [ray (ray/ray [0.0 0.0 0.75 1.0] [0.0 0.0 -1.0 0.0])
          world-mat-1 (assoc-in world/default-world [:shapes 0 :material :ambient] 1.0)
          world-mat-2 (assoc-in world-mat-1 [:shapes 1 :material :ambient] 1.0)
          color-inner-material-func (get-in world-mat-2 [:shapes 1 :material :color])
          color-inner-material (color-inner-material-func (util/point 0 0 0))
          color (world/color-at ray world-mat-2)]
      (is (= color color-inner-material))
      )
    )
  )

(deftest cast-shadows
  (let [w world/default-world]
    (testing "There is no shadow when nothing os collinear with point and light"
      (is (= (world/is-shadowed w (util/point 0 10 0)) false))
      )

    (testing "The shadow when an object is between the point and the light"
      (is (= (world/is-shadowed w (util/point 10 -10 10)) true))
      )

    (testing "There is no shadow when an object is behind the light"
      (is (= (world/is-shadowed w (util/point -20 20 -20)) false))
      )

    (testing "There is no shadow when an object is behind the point"
      (is (= (world/is-shadowed w (util/point -2 2 -2)) false))
      )
    )
  )
