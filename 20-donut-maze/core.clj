(ns donut-maze.core
  (:require [clojure.string :as string]))

(defn read-map [s]
  (->> (string/split s #"\n")
       (filter #(> (count %) 0))
       (map-indexed (fn [y line] (map-indexed (fn [x c] [[x y] c]) line)))
       (apply concat)
       (reduce (fn [m [coordinates c]] (assoc-in m coordinates c)) {})))

(defn whats-at
  [m coordinates]
  (get-in m coordinates \space))

(defn m->seq [m]
  (mapcat (fn [[x cs]] (map (fn [[y c]] [x y c]) cs)) m))

(defn find-item
  "Finds the first item, going down the columns, left to right"
  [m item]
  (let [[x y c]
        (->> (m->seq m)
             sort
             (filter #(= item (nth % 2)))
             first)]
    [x y]))

(defn neighbors
  [[x y]]
  [[x (dec y)] ;; top
   [(inc x) y] ;; right
   [x (inc y)] ;; bottom
   [(dec x) y] ;; left
   ])

(def portal-chars
  (set (map char (range 65 (+ 65 26)))))

(defn donut-top-left [m]
  (find-item m \#))

(defn donut-inner-top-left [m]
  (let [[x y _]
        (->> (m->seq m)
             ;; Swap so that we're scanning left to right, top to bottom
             (map (fn [[x y c]] [y x c]))
             sort
             (remove (fn [[x y c]] (or (< x 3) (< y 3))))
             (filter #(= \space (nth % 2)))
             first)]
    [(dec x) (dec y)]))

(defn coordinate-label
  "Gets the label for coordinates, which is [<set of letters> <coordinates>]
   We use coordinates to differentiate the two ends of a portal
   (because warping takes 1 step)"
  [m coordinates]
  (let [c0 (->> (neighbors coordinates)
                (filter #(portal-chars (whats-at m %)))
                first)]
    [(set [(whats-at m (map + (map - c0 coordinates) c0)) (whats-at m c0)]) coordinates]))

(defn is-labeled?
  [m coordinates]
  (and (= \. (whats-at m coordinates))
       (some portal-chars (map (partial whats-at m) (neighbors coordinates)))))

(defn num-portals [m]
  (/ (->> (m->seq m)
          (filter #(portal-chars (nth % 2)))
          count) 2))

(defn all-labeled-coordinates [m]
  (->> (m->seq m)
       (map (fn [[x y c]] [x y]))
       (filter (fn [[x y]] (is-labeled? m [x y])))
       (map (fn [[x y]] (coordinate-label m [x y])))))

(defn neighboring-steps
  [m coordinates]
  (->> (neighbors coordinates)
       (filter (fn [c] (= \. (whats-at m c))))))

;; TODO check that order of characters in portal names don't matter
;;   (there shouldn't be a portal AB and a portal BA)

(defn step
  [m {:keys [coordinates visited distance]}]
  (->> (neighboring-steps m coordinates)
       (remove visited)
       (map (fn [c] {:coordinates c
                     :visited (conj visited c)
                     :distance (inc distance)}))))

(defn edge-from-work-item
  [m src {:keys [coordinates distance]}]
  [src (coordinate-label m coordinates) distance])

(defn map-from
  [m coordinates]
  (let [src (coordinate-label m coordinates)]
    (loop [work-list [{:coordinates coordinates
                       :visited #{coordinates}
                       :distance 0}]
           ;; vector of src, dst, distance
           results []]
      (let [portals (filter (comp (partial is-labeled? m) :coordinates) work-list)
            next-results (concat results (map #(edge-from-work-item m src %) portals))]
        (if (seq work-list)
          (recur (mapcat (partial step m) work-list) next-results)
          next-results)))))

(defn graph [m]
  (let [labeled-coordinates (->> (all-labeled-coordinates m)
                                 (map second))
        labels (map (partial coordinate-label m) labeled-coordinates)
        portals-grouped (->> (group-by first labels)
                             (filter (fn [[k v]] (= 2 (count v)))))
        warp-edges (->> portals-grouped
                        vals
                        (mapcat (fn [[a b]] [[a b 1] [b a 1]])))
        edges (->> (mapcat (partial map-from m) labeled-coordinates)
                   (remove #(zero? (nth % 2))))]
    (->> (concat warp-edges edges)
         (reduce (fn [m [src dst distance]] (assoc-in m [src dst] distance)) {}))))

(defn update-distances
  [distances current-distance [dst delta]]
  (let [new-dist (+ current-distance delta)]
    (update distances dst (fn [d] (if d (min d new-dist) new-dist)))))

(defn shortest-distance-from-AA-to-ZZ [m]
  (let [g (graph m)
        nodes (set (keys g))
        start (first (filter #(= #{\A} (first %)) nodes))
        end (first (filter #(= #{\Z} (first %)) nodes))]
    (loop [curr start
           unvisited nodes
           distances {start 0}]
      (let [edges (g curr)
            curr-distance (distances curr)
            next-distances (reduce
                            (fn [m edge] (update-distances m curr-distance edge))
                            distances
                            edges)
            next-unvisited (disj unvisited curr)]
        (if (seq next-unvisited)
          (let [next-curr (->> (filter #(next-unvisited (first %)) next-distances)
                               (apply min-key second)
                               first)]
            (recur next-curr next-unvisited next-distances))
          (distances end))))))

(def test-map-0 "
         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
DE..#######...###.#
  #.#########.###.#
FG..#########.....#
  ###########.#####
             Z
             Z
")

(def test-map-1 "
                   A
                   A
  #################.#############
  #.#...#...................#.#.#
  #.#.#.###.###.###.#########.#.#
  #.#.#.......#...#.....#.#.#...#
  #.#########.###.#####.#.#.###.#
  #.............#.#.....#.......#
  ###.###########.###.#####.#.#.#
  #.....#        A   C    #.#.#.#
  #######        S   P    #####.#
  #.#...#                 #......VT
  #.#.#.#                 #.#####
  #...#.#               YN....#.#
  #.###.#                 #####.#
DI....#.#                 #.....#
  #####.#                 #.###.#
ZZ......#               QG....#..AS
  ###.###                 #######
JO..#.#.#                 #.....#
  #.#.#.#                 ###.#.#
  #...#..DI             BU....#..LF
  #####.#                 #.#####
YN......#               VT..#....QG
  #.###.#                 #.###.#
  #.#...#                 #.....#
  ###.###    J L     J    #.#.###
  #.....#    O F     P    #.#...#
  #.###.#####.#.#####.#####.###.#
  #...#.#.#...#.....#.....#.#...#
  #.#####.###.###.#.#.#########.#
  #...#.#.....#...#.#.#.#.....#.#
  #.###.#####.###.###.#.#.#######
  #.#.........#...#.............#
  #########.###.###.#############
           B   J   C
           U   P   P
")

;; Part 1

(shortest-distance-from-AA-to-ZZ (read-map (slurp "./20-donut-maze/input.txt")))

;; => 618
