(ns clojureray.pattern
  (:require [clojureray.util :as util]))

(defn stripe-pattern
  [color1 color2]
  (fn [object p] (let [p-x (get p 0)
                       check (even? (int (Math/floor p-x)))]
                   (if check color1 color2)
                   )
    )
  )
