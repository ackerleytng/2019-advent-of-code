(ns many-worlds.core
  (:require [clojure.string :as s]))

(defn read-map
  [string]
  (->> (s/split string #"\n")
       (map s/trim)
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

(defn start-coordinates [m]
  (let [[_ x y]
        (->> (mapcat (fn [[y cs]] (map (fn [[x c]] [c x y]) cs)) m)
             (filter #(= \@ (first %)))
             first)]
    [x y]))

(def puzzle-keys
  (set (map char (take 26 (iterate inc (int \a))))))

(def puzzle-doors
  (set (map char (take 26 (iterate inc (int \A))))))

(def puzzle-walls #{\#})

(defn count-keys [m]
  (->> (mapcat (fn [[y cs]] (map (fn [[x c]] [c x y]) cs)) m)
       (filter (comp puzzle-keys first))
       count))

(defn has-key?
  [keys-held key]
  ((set keys-held) key))

(defn has-key-for-door?
  [keys-held door]
  ((set keys-held) (char (+ (int door) 32))))

(defn whats-at
  [m [x y]]
  (get-in m [y x]))

(defn non-wall-moves
  [m coordinates]
  (->> (moves coordinates)
       ;; Ensure that the move is on the map
       (filter #(whats-at m %))
       (remove #(puzzle-walls (whats-at m %)))))

(defn explore
  [m num-keys {:keys [coordinates keys-held visited distance]}]
  {:pre [(not (puzzle-walls (whats-at m coordinates)))
         (not (= num-keys (count keys-held)))]}
  (let [at-coordinates (whats-at m coordinates)
        proceed (cond
                  (puzzle-doors at-coordinates) (has-key-for-door? keys-held at-coordinates)
                  :else true)]
    (when proceed
      (let [next-keys-held (if (and (puzzle-keys at-coordinates) (not (has-key? keys-held at-coordinates)))
                             ;; Pick up the key
                             (conj keys-held at-coordinates)
                             keys-held)
            next-visited (if (and (puzzle-keys at-coordinates) (not (has-key? keys-held at-coordinates)))
                           ;; Reset - can explore backwards after picking up key
                           #{coordinates}
                           ;; If you didn't pick up a key, keep going in original direction
                           (conj visited coordinates))
            next-coordinates (remove next-visited (non-wall-moves m coordinates))]
        (map
         (fn [c] {:coordinates c
                  :keys-held next-keys-held
                  :visited next-visited
                  :distance (inc distance)})
         next-coordinates)))))

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

(defn shortest-path-that-collects-all-keys
  [m]
  (let [start (start-coordinates m)
        num-keys (count-keys m)]
    (loop [work-list
           (->> (non-wall-moves m start)
                ;; Work items are built here
                (map (fn [coordinates]
                       {:coordinates coordinates
                        ;; Stored as vector because we want to retain order
                        :keys-held []
                        :visited #{start}
                        :distance 0})))]
      (let [next-work-list (mapcat (partial explore m num-keys) work-list)]
        (if (zero? (mod (:distance (first next-work-list)) 10))
          (println (:distance (first next-work-list))))
        #_(println (map #(dissoc % :visited) next-work-list))
        #_(Thread/sleep 300)
        (if (some #(= num-keys (count (:keys-held %))) next-work-list)
          (:distance (first (filter #(= num-keys (count (:keys-held %))) next-work-list)))
          (recur next-work-list))))))

(shortest-path-that-collects-all-keys (read-map test-map-2))
