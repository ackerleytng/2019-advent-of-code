(ns monitoring-station.core
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer [pprint]]))

(defn ->x-coords
  [line]
  (->> (map vector line (range))
       (filter #(= (first %) \#))
       (map second)))

(defn ->coordinates
  [string]
  (->> (s/split string #"\n")
       (map (comp ->x-coords s/trim))
       (mapcat (fn [y xs] (map (fn [x] [x y]) xs)) (range))))

(defn read-image
  [filename]
  (->coordinates (slurp filename)))

(defn gradient
  [[x0 y0] [x1 y1]]
  (cond
    (and (= x1 x0) (= y1 y0)) (throw (IllegalArgumentException. "Coordinates are equal!"))
    (= x1 x0) :vertical
    :else (/ (- y1 y0) (- x1 x0))))

(defn all-gradients
  "Computes a sequence of tuples: (set of all asteroids that have this gradient, gradient, sequence of vectors),
   For the sequence of vectors, in each vector, the first asteroid is always the first one encountered while iterating from top to bottom, left to right"
  [coordinates]
  (->> (group-by #(apply gradient %) (combo/combinations coordinates 2))
       (map (fn [[gradient sequence]]
              [(set (mapcat #(apply vector %) sequence)) gradient sequence]))))

(defn all-gradients
  [coordinates]
  (group-by #(apply gradient %) (combo/combinations coordinates 2)))

(combo/combinations [1 2 3] 2)

(gradient [0 0] [0 1])
(gradient [2 2] [1 1])

(defn coordinates-are-in-first-position
  [coordinates sequence]
  (some (fn [vector] (= (first vector) coordinates)) sequence))

(defn coordinates-are-in-second-position
  [coordinates sequence]
  (some (fn [vector] (= (second vector) coordinates)) sequence))

(defn count-gradients
  [[asteroid-set gradient sequence] coordinates]
  (if (asteroid-set coordinates)
    (+ (if (coordinates-are-in-first-position coordinates sequence) 1 0) (if (coordinates-are-in-second-position coordinates sequence) 1 0))
    0))

(defn count-gradients-for-coordinates
  [coordinates gradient-info]
  (apply + (map #(count-gradients % coordinates) gradient-info)))

(defn best-location-coverage
  [coordinates]
  (let [gradient-info (all-gradients coordinates)]
    (apply
     max (map
          #(count-gradients-for-coordinates % gradient-info)
          coordinates))))

(comment

  (def sample-coordinates
    (->coordinates
     ".#..#
    .....
    #####
    ....#
    ...##"))

  (let [gradient-info (all-gradients sample-coordinates)]
    (map
     (fn [coordinates] [coordinates (count-gradients-for-coordinates coordinates gradient-info)])
     sample-coordinates))

  ;; => ([[1 0] 7] [[4 0] 7] [[0 2] 6] [[1 2] 7] [[2 2] 7] [[3 2] 7] [[4 2] 5] [[4 3] 7] [[3 4] 8] [[4 4] 7])

  (best-location-coverage sample-coordinates)

  (count-gradients-for-coordinates [1 1] (all-gradients [[0 0] [1 1] [2 2]]))

  (count-gradients (first (all-gradients [[0 0] [1 1] [2 2]])) [0 0])

  )

(def test-0-coordinates
  (->coordinates
   "......#.#.
    #..#.#....
    ..#######.
    .#.#.###..
    .#..#.....
    ..#....#.#
    #..#....#.
    .##.#..###
    ##...#..#.
    .#....####"))

(def test-1-coordinates
  (->coordinates
   "#.#...#.#.
    .###....#.
    .#....#...
    ##.#.#.#.#
    ....#.#.#.
    .##..###.#
    ..#...##..
    ..##....##
    ......#...
    .####.###."))

(def test-2-coordinates
  (->coordinates
   ".#..#..###
    ####.###.#
    ....###.#.
    ..###.##.#
    ##.##.#.#.
    ....###..#
    ..#.#..#.#
    #..#.#.###
    .##...##.#
    .....#.#.."))

(def test-3-coordinates
  (->coordinates
   ".#..##.###...#######
    ##.############..##.
    .#.######.########.#
    .###.#######.####.#.
    #####.##.#.##.###.##
    ..#####..#.#########
    ####################
    #.####....###.#.#.##
    ##.#################
    #####.##.###..####..
    ..######..##.#######
    ####.##.####...##..#
    .#####..#.######.###
    ##...#.##########...
    #.##########.#######
    .####.#.###.###.#.##
    ....##.##.###..#####
    .#.#.###########.###
    #.#.#.#####.####.###
    ###.##.####.##.#..##"))

;; Part 1

(comment

  (best-location-coverage (read-image "./10-monitoring-station/input.txt"))

  )

;; => 319

;; Part 2

(defn best-location
  [coordinates]
  (let [gradient-info (all-gradients coordinates)]
    (second
     (apply
      max-key first
      (map
       (fn [coordinates] [(count-gradients-for-coordinates coordinates gradient-info) coordinates])
       coordinates)))))

(comment

  (best-location (read-image "./10-monitoring-station/input.txt"))

  ;; => [31 20]

  )

(defn distance
  ([coordinates] (distance [0 0] coordinates))
  ([[x0 y0] [x1 y1]]
   (let [x (- x1 x0)
         y (- y1 y0)]
     (Math/hypot x y))))

(defn angle
  [[x0 y0] [x1 y1]]
  (let [x (- x1 x0)
        y (- y0 y1)]
    (let [rad (Math/atan2 x y)]
      (if (< rad 0)
        (+ (* 2 Math/PI) rad)
        rad))))

(def test-4-coordinates
  (->coordinates
   ".#....#####...#..
    ##...##.#####..##
    ##...#...#.#####.
    ..#.....#...###..
    ..#.#.....#....##"))

(defn nth-to-be-vaporized
  [coordinates monitoring-station n]
  (let [asteroids-sorted
        (->> (remove #{monitoring-station} coordinates)
             (group-by (partial angle monitoring-station))
             sort
             (map (fn [[_ coords]] (sort-by (partial distance monitoring-station) coords))))
        max-count (apply max (map count asteroids-sorted))]
    (->> asteroids-sorted
         (map #(concat % (take max-count (repeat :filler))))
         (apply mapcat vector)
         (remove #{:filler})
         (take n)
         last)))

(comment

  (nth-to-be-vaporized test-3-coordinates [11 13] 200)

  )

(nth-to-be-vaporized (read-image "./10-monitoring-station/input.txt") [31 20] 200)

;; => [5 17]
