(ns mass.core
  (:require [clojure.string :as s]))

(defn compute-fuel-req [mass]
  (- (quot mass 3) 2))

(defn compute-total-fuel-req [mass]
  (->> (iterate compute-fuel-req mass)
       (take-while pos?)
       (drop 1)
       (apply +)))

;; Part 1
(->> (s/split (slurp "./1-mass/input.txt") #"\n")
     (map #(compute-fuel-req (Integer/parseInt %)))
     (apply +))
;; => 3320816

;; Part 2
(->> (s/split (slurp "./1-mass/input.txt") #"\n")
     (map #(compute-total-fuel-req (Integer/parseInt %)))
     (apply +))
;; => 4978360
