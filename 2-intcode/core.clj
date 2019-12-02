(ns intcode.core
  (:require [clojure.string :as s]))

(defn instruction [idx code]
  (take 4 (nthnext code idx)))

(defn evaluate
  "Given [operation code], evaluate operation on code and return new code"
  [[int-code idx-0 idx-1 idx-result] code]
  (let [res (case int-code
              1 (+ (nth code idx-0) (nth code idx-1))
              2 (* (nth code idx-0) (nth code idx-1)))]
    (assoc code idx-result res)))

(defn interpret [code]
  (loop [idx 0
         state code]
    (if (= (nth state idx) 99)
      (nth state 0)
      (recur (+ idx 4) (evaluate (instruction idx state) state)))))

(defn input-args [arg-1 arg-2 code]
  (-> code
      (assoc 1 arg-1)
      (assoc 2 arg-2)))

;; Read inputs

(def input-code
  (->> (s/split (slurp "./2-intcode/input.txt") #",")
       (map #(Integer/parseInt (s/trim %)))
       vec))

;; Part 1

(->> input-code
     (input-args 12 2)
     interpret)
;; => 10566835

;; Part 2

(let [[_ noun verb]
      (->> (for [arg-1 (range 100)
                 arg-2 (range 100)]
             [(interpret (input-args arg-1 arg-2 input-code)) arg-1 arg-2])
           (filter #(= 19690720 (first %)))
           first)]
  (+ (* 100 noun) verb))
;; => 2347
