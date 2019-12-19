(ns many-worlds.core
  (:require [clojure.string :as string]
            [clojure.set :as s]
            [clojure.pprint :refer [pprint]]))

(defn read-map
  [string]
  (->> (string/split string #"\n")
       (map string/trim)
       (remove #(zero? (count %)))
       (map-indexed
        (fn [i line]
          [i (into {} (map-indexed (fn [i c] [i c]) line))]))
       (into {})))

(defn moves
  [coordinates]
  (map #(map + coordinates %)
       [[0 1]
        [1 0]
        [0 -1]
        [-1 0]]))

(defn m->seq [m]
  (mapcat (fn [[y cs]] (map (fn [[x c]] [c x y]) cs)) m))

(defn find-item [m item]
  (let [[_ x y]
        (->> (m->seq m)
             (filter #(= item (first %)))
             first)]
    [x y]))

(defn start-coordinates [m]
  (find-item m \@))

(def puzzle-keys
  (set (map char (take 26 (iterate inc (int \a))))))

(def puzzle-doors
  (set (map char (take 26 (iterate inc (int \A))))))

(def puzzle-walls #{\#})

(defn count-keys [m]
  (->> (m->seq m)
       (filter (comp puzzle-keys first))
       count))

(defn has-key?
  [keys-held key]
  ((set keys-held) key))

(defn has-key-for-door?
  [keys-held door]
  ((set keys-held) (char (+ (int door) 32))))

(defn have-keys-for-doors?
  [keys-held doors]
  (let [required-key-set (map #(char (+ (int %) 32)) doors)]
    (s/subset? required-key-set (set keys-held))))

(defn whats-at
  [m [x y]]
  (get-in m [y x]))

(defn non-wall-moves
  [m coordinates]
  (->> (moves coordinates)
       ;; Ensure that the move is on the map
       (filter #(whats-at m %))
       (remove #(puzzle-walls (whats-at m %)))))

(defn step
  [m {:keys [coordinates distance doors-to-pass visited]}]
  (let [at-coordinates (whats-at m coordinates)]
    (->> (non-wall-moves m coordinates)
         (remove visited)
         (map (fn [c]
                {:coordinates c
                 :distance (inc distance)
                 :doors-to-pass (if (puzzle-doors at-coordinates)
                                  (conj doors-to-pass at-coordinates)
                                  doors-to-pass)
                 :visited (conj visited coordinates)})))))

(defn map-from
  [m puzzle-key]
  (let [key-coordinates (find-item m puzzle-key)]
    (loop [work-list [{:coordinates key-coordinates
                       :distance 0
                       :doors-to-pass #{}
                       :visited #{}}]
           paths []]
      (let [found (filter
                   (fn [{:keys [coordinates]}]
                     (let [at-coordinates (whats-at m coordinates)]
                       (and (puzzle-keys at-coordinates) (not= puzzle-key at-coordinates))))
                   work-list)
            next-paths (concat paths found)]
        #_(println "w" (map (fn [w]
                              (assoc (dissoc w :visited) :i (whats-at m (:coordinates w))))
                            work-list))
        (if (seq work-list)
          (recur
           (mapcat (partial step m) work-list)
           next-paths)
          (map (fn [w] (assoc (select-keys w [:distance :doors-to-pass])
                              :key-found (whats-at m (:coordinates w))))
               next-paths))))))

(defn path->edge
  [src {:keys [key-found] :as path}]
  (assoc (dissoc path :key-found) :nodes #{src key-found}))

(defn edges [m & puzzle-starts]
  (let [key-list (->> (m->seq m)
                      (map first)
                      (filter puzzle-keys))
        key-list-with-start (apply conj key-list puzzle-starts)
        coordinates (map #(find-item m %) key-list-with-start)]
    (set (mapcat
          (fn [k] (map (partial path->edge k) (map-from m k)))
          key-list-with-start))))

(def test-map-0 "
#########
#b.A.@.a#
#########
")

(def test-map-1 "
########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
")

(def test-map-2 "
########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################
")

(def test-map-3 "
#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################
")

(def test-map-4 "
########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################
")

;; (edges (read-map test-map-2) \@)

(defn edges->adj-list
  [edges]
  (let [nodes (apply s/union (map :nodes edges))]
    (->> (map (fn [n] [n (->> edges
                              (filter #((:nodes %) n))
                              (map (fn [e] (update e :nodes #(first (s/difference % #{n})))))
                              (remove #(= \@ (:nodes %))))]) nodes)
         (into {}))))

(defn permitted-edges
  [graph keys-held node]
  (->> (graph node)
       (filter #(have-keys-for-doors? keys-held (:doors-to-pass %)))))

(defn possible-realities
  [graph {:keys [node distance keys-held visited]}]
  (->> (permitted-edges graph keys-held node)
       (remove #(visited (:nodes %)))
       (map (fn [{nodes :nodes d :distance}]
              {:node nodes
               :distance (+ distance d)
               :keys-held (if (puzzle-keys nodes) (conj keys-held nodes) keys-held)
               :visited (conj visited nodes)}))))

(defn trim-realities
  [realities]
  (->> realities
       (group-by #(select-keys % [:visited :node]))
       (map (fn [[k v]] (apply min-key :distance v)))))

(defn shortest-path-that-collects-all-keys [m]
  (let [num-keys (count-keys m)
        graph (edges->adj-list (edges m \@))]
    (loop [realities [{:node \@
                       :distance 0
                       ;; Using a vector to retain order
                       :keys-held []
                       :visited #{\@}}]]
      (let [all-next-realities (mapcat (partial possible-realities graph) realities)
            next-realities (trim-realities all-next-realities)]
        (pprint ["a" all-next-realities])
        (pprint ["n" next-realities])
        #_(pprint (- (count all-next-realities) (count next-realities)))
        #_(Thread/sleep 300)
        (if (some #(= num-keys (count (:keys-held %))) next-realities)
          (apply min (map :distance next-realities))
          (recur next-realities))))))

;; Part 1

(comment

  (shortest-path-that-collects-all-keys (read-map test-map-2))

  (shortest-path-that-collects-all-keys (read-map (slurp "./18-many-worlds/input.txt")))

  )

;; => 2684

(def test-map-5-should-patch "
#######
#a.#Cd#
##...##
##.@.##
##...##
#cB#Ab#
#######
")

(def test-map-6 "
###############
#d.ABC.#.....a#
######$#&######
###############
######%#@######
#b.....#.....c#
###############
")

(def test-map-7 "
#############
#DcBa.#.GhKl#
#.###$#&#I###
#e#d#####j#k#
###C#%#@###J#
#fEbA.#.FgHi#
#############
")

(def test-map-8 "
#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba$#&BcIJ#
#############
#nK.L%#@G...#
#M###N#H###.#
#o#m..#i#jk.#
#############
")

(defn m->robot-m
  [m]
  (let [[x y] (start-coordinates m)]
    (-> m
        (assoc-in [y x] \#)
        (assoc-in [(inc y) x] \#)
        (assoc-in [(dec y) x] \#)
        (assoc-in [y (inc x)] \#)
        (assoc-in [y (dec x)] \#)
        (assoc-in [(inc y) (inc x)] \@)
        (assoc-in [(inc y) (dec x)] \%)
        (assoc-in [(dec y) (inc x)] \&)
        (assoc-in [(dec y) (dec x)] \$))))

(def puzzle-starts
  [\$ \&
   \% \@])

(defn replace
  [v before after]
  (assoc v (.indexOf v before) after))

(defn merge-reality
  [{:keys [nodes visiteds] :as full}
   src
   {:keys [node distance keys-held visited]}]
  (let [pos (.indexOf nodes src)]
    {:nodes (assoc nodes pos node)
     :distance distance
     :keys-held keys-held
     :visiteds (assoc visiteds pos visited)}))

(defn possible-realities-2
  [graph {:keys [nodes visiteds keys-held distance] :as reality}]
  (let [robot-realities
        (map (fn [n v]
               (map (partial merge-reality reality n)
                    (possible-realities
                     graph
                     {:node n
                      :distance distance
                      :keys-held keys-held
                      :visited v})))
             nodes visiteds)]
    (apply concat robot-realities)))

(defn trim-realities-2
  [realities]
  (->> realities
       (group-by (juxt (comp set :keys-held) :nodes))
       (map (fn [[k v]] (apply min-key :distance v)))))

(defn shortest-path-that-collects-all-keys-2 [m]
  (let [num-keys (count-keys m)
        graph (edges->adj-list (apply edges m puzzle-starts))]
    (loop [realities [{:nodes puzzle-starts
                       :distance 0
                       ;; Using a vector to retain order
                       :keys-held []
                       :visiteds (vec (map (fn [s] #{s}) puzzle-starts))}]]
      (let [all-next-realities (mapcat (partial possible-realities-2 graph) realities)
            next-realities (trim-realities-2 all-next-realities)]
        #_(pprint ["a" all-next-realities])
        #_(pprint ["n" next-realities])
        #_(Thread/sleep 300)
        (if (some #(= num-keys (count (:keys-held %))) next-realities)
          (apply min (map :distance next-realities))
          (recur next-realities))))))

;; Part 2

(comment

  (shortest-path-that-collects-all-keys-2 (m->robot-m (read-map test-map-5-should-patch)))

  (shortest-path-that-collects-all-keys-2 (read-map test-map-8))

  (shortest-path-that-collects-all-keys-2 (m->robot-m (read-map (slurp "./18-many-worlds/input.txt"))))

  )

;; => 1886
