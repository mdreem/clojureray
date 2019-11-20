(ns clojureray.comparison)

(defprotocol AlmostEqual
  (aeq [x y]))

(extend java.lang.Double
  AlmostEqual
  {:aeq (fn [d1 d2]
        (< (Math/abs (clojure.core/- d1 d2)) 0.00001))})

(extend clojure.lang.PersistentVector
  AlmostEqual
  {:aeq (fn [v1 v2]
        (= (count v1) (count v2))
        (every? true? (map aeq v1 v2)))})
