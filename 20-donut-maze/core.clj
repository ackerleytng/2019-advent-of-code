(ns donut-maze.core
  (:require [clojure.string :as string]
            [clojure.set :as s]
            [clojure.data.priority-map :refer [priority-map]]))

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

(defn all-labeled-coordinates [m]
  (->> (m->seq m)
       (map (fn [[x y c]] [x y]))
       (filter (fn [[x y]] (is-labeled? m [x y])))
       (map (fn [[x y]] (coordinate-label m [x y])))))

(defn neighboring-steps
  [m coordinates]
  (->> (neighbors coordinates)
       (filter (fn [c] (= \. (whats-at m c))))))

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

(defn shortest-distance-from-AA-to-ZZ [g]
  (let [nodes (set (keys g))
        start (first (filter #(= #{\A} (first %)) nodes))
        end (first (filter #(= #{\Z} (first %)) nodes))
        pq (priority-map start 0)
        distances {start 0}]
    (loop [pq pq
           distances distances]
      (let [[curr curr-distance] (peek pq)
            edges (g curr)
            next-distances (reduce
                            (fn [m edge] (update-distances m curr-distance edge))
                            distances edges)
            changes (filter (fn [[dst delta]] (not= (next-distances dst) (distances dst))) edges)
            changes-distances (map
                               (fn [[dst delta]] [dst (next-distances dst)])
                               changes)
            next-pq (into (pop pq) changes-distances)]
        (if (seq next-pq)
          (recur next-pq next-distances)
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

(shortest-distance-from-AA-to-ZZ (graph (read-map test-map-1)))

(shortest-distance-from-AA-to-ZZ (graph (read-map (slurp "./20-donut-maze/input.txt"))))

;; => 618

(def test-map-2 "
             Z L X W       C
             Z P Q B       K
  ###########.#.#.#.#######.###############
  #...#.......#.#.......#.#.......#.#.#...#
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###
  #.#...#.#.#...#.#.#...#...#...#.#.......#
  #.###.#######.###.###.#.###.###.#.#######
  #...#.......#.#...#...#.............#...#
  #.#########.#######.#.#######.#######.###
  #...#.#    F       R I       Z    #.#.#.#
  #.###.#    D       E C       H    #.#.#.#
  #.#...#                           #...#.#
  #.###.#                           #.###.#
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#
CJ......#                           #.....#
  #######                           #######
  #.#....CK                         #......IC
  #.###.#                           #.###.#
  #.....#                           #...#.#
  ###.###                           #.#.#.#
XF....#.#                         RF..#.#.#
  #####.#                           #######
  #......CJ                       NM..#...#
  ###.#.#                           #.###.#
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#
  #.....#        F   Q       P      #.#.#.#
  ###.###########.###.#######.#########.###
  #.....#...#.....#.......#...#.....#.#...#
  #####.#.###.#######.#######.###.###.#.#.#
  #.......#.......#.#.#.#.#...#...#...#.#.#
  #####.###.#####.#.#.#.#.###.###.#.###.###
  #.......#.....#.#...#...............#...#
  #############.#.#.###.###################
               A O F   N
               A A D   M
")

;; Approach
;; Find the length of the path from AA to ZZ (without recursion)
;; The depth of the recursion can't be greater than
;;   half the length of that path, because we have to
;;   recurse down and up again. If it were longer, then the direct path
;;   would be shorter anyway, and we want the shortest path.
;; So, we can generate all the nodes for all the levels,
;;   then use dijkstra's to find the shortest path

(defn on-outer-edge?
  "Determines if this set of coordinates is on the outer edge.
   Technically doesn't work for all cases - for example, if the
   map is such that the right edge is only 1 unit thick.
   For the maps that I'll be operating on, this function will work."
  [m [x y]]
  (let [x-max (->> (m->seq m)
                   (apply max-key (fn [[x y _]] x))
                   first)
        y-max (->> (m->seq m)
                   (apply max-key (fn [[x y _]] y))
                   second)]
    (or (> x (- x-max 4)) (< x 4)
        (> y (- y-max 4)) (< y 4))))

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

(defn add-level-to-edge
  [level [src dst dist]]
  [(update src 1 conj level) (update dst 1 conj level) dist])

(defn warp-edge
  [m outer-level portals]
  (let [[[outer] [inner]] ((juxt filter remove) #(on-outer-edge? m (second %)) portals)
        inner (update inner 1 conj outer-level)
        outer (update outer 1 conj (inc outer-level))]
    [;; Warp from portal on inner edge to a deeper level
     [inner outer 1]
     ;; Warp from portal on outer edge back to a portal on the inner edge on outer level
     [outer inner 1]]))

(defn graph-deep
  [m num-levels]
  (let [labeled-coordinates (->> (all-labeled-coordinates m)
                                 (map second))
        labels (map (partial coordinate-label m) labeled-coordinates)
        start (first (filter #(= #{\A} (first %)) labels))
        end (first (filter #(= #{\Z} (first %)) labels))
        portals-grouped (->> (group-by first labels)
                             (filter (fn [[k v]] (= 2 (count v)))))
        portals (->> portals-grouped
                     vals
                     (apply concat))
        [outer-portals inner-portals]
        ((juxt filter remove) #(on-outer-edge? m (second %)) portals)

        ;; 0th level: only edges among AA and ZZ, and all inner-portals
        level-0-portals (conj (set inner-portals) start end)
        level-0-edges (->> (mapcat (partial map-from m) labeled-coordinates)
                           (remove #(zero? (nth % 2)))
                           (filter (fn [[src dst _]] (every? level-0-portals [src dst])))
                           (map (partial add-level-to-edge 0)))

        within-level-edges (->> (map second portals)
                                (mapcat (partial map-from m))
                                (remove #(zero? (nth % 2)))
                                ;; Remove edges going to AA and ZZ
                                (remove (fn [[src dst _]] (some #{start end} [src dst]))))
        all-within-level-edges
        (mapcat #(map (partial add-level-to-edge %) within-level-edges) (range 1 (inc num-levels)))
        all-warp-edges
        (mapcat #(mapcat (fn [[_ ps]] (warp-edge m % ps)) portals-grouped) (range num-levels))]
    (->> (concat level-0-edges all-warp-edges all-within-level-edges)
         (reduce (fn [m [src dst distance]] (assoc-in m [src dst] distance)) {}))))

;; Part 2

(time
 (let [m (read-map test-map-2)
       no-recursion-min-distance (shortest-distance-from-AA-to-ZZ (graph m))
       num-levels (inc (quot no-recursion-min-distance 2))]
   (shortest-distance-from-AA-to-ZZ (graph-deep m num-levels))))

(comment

  ;; Takes pretty long on the actual one

  (time
   (let [m (read-map (slurp "./20-donut-maze/input.txt"))
         no-recursion-min-distance (shortest-distance-from-AA-to-ZZ (graph m))
         num-levels (inc (quot no-recursion-min-distance 2))]
     (shortest-distance-from-AA-to-ZZ (graph-deep m num-levels))))

  )

;; => 7152
