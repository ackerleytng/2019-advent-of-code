(ns crossed-wires.core
  (:require [clojure.string :as string]
            [clojure.set :as s]))

(defn parse-segment
  [segment]
  (let [direction (re-find #"[UDLR]" segment)
        step-count (Integer/parseInt (re-find #"\d+" segment))]
    [direction step-count]))

(defn trace-segment
  [[x y] segment]
  (let [[direction step-count] (parse-segment segment)]
    (case direction
      "U" (map (fn [n] [x (+ y n)]) (range 1 (inc step-count)))
      "D" (map (fn [n] [x (- y n)]) (range 1 (inc step-count)))
      "L" (map (fn [n] [(- x n) y]) (range 1 (inc step-count)))
      "R" (map (fn [n] [(+ x n) y]) (range 1 (inc step-count))))))

(defn trace-path
  "Trace an entire path beginning with [0 0], where a path is a vector of segments"
  [path]
  (reduce
   (fn [coordinates segment]
     (concat coordinates (trace-segment (last coordinates) segment)))
   [[0 0]] path))

(defn closest-intersection
  [path-0 path-1]
  (let [trace-0 (trace-path path-0)
        trace-1 (trace-path path-1)
        intersections
        (disj (s/intersection (set trace-0) (set trace-1)) [0 0])]
    (->> intersections
         (map (fn [[a b]] (+ (Math/abs a) (Math/abs b))))
         (apply min))))

;; Exercising the functions

(closest-intersection ["R8" "U5" "L5" "D3"] ["U7" "R6" "D4" "L4"])

(closest-intersection
 ["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"]
 ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"])

(closest-intersection
 ["R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"]
 ["U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7"])

;; Read inputs

(def input-code
  (->> (string/split (slurp "./3-crossed-wires/input.txt") #"\n")
       (map #(string/split % #","))))

;; Part 1

(apply closest-intersection input-code)

;; => 221


;; Part 2 additional functions

(defn signal-delay
  [trace coordinates]
  (.indexOf trace coordinates))

(s/intersection (set (trace-path ["R8" "U5" "L5" "D3"])) (set (trace-path ["U7" "R6" "D4" "L4"])))
;; => #{[0 0] [3 3] [6 5]}

(signal-delay (trace-path ["R8" "U5" "L5" "D3"]) [3 3])
;; => 20

(signal-delay (trace-path ["R8" "U5" "L5" "D3"]) [6 5])
;; => 15

(defn closest-intersection-by-signal-delay
  [path-0 path-1]
  (let [trace-0 (trace-path path-0)
        trace-1 (trace-path path-1)
        intersections
        (disj (s/intersection (set trace-0) (set trace-1)) [0 0])]
    (->> intersections
         (map (fn [coordinates]
                (+ (signal-delay trace-0 coordinates)
                   (signal-delay trace-1 coordinates))))
         (apply min))))

;; Exercising functions

(closest-intersection-by-signal-delay ["R8" "U5" "L5" "D3"] ["U7" "R6" "D4" "L4"])

(closest-intersection-by-signal-delay
 ["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"]
 ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"])

(closest-intersection-by-signal-delay
 ["R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"]
 ["U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7"])


;; Part 2

(apply closest-intersection-by-signal-delay input-code)

;; => 18542
