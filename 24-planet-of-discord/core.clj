(ns planet-of-discord.core
  (:require [clojure.string :as string]))

(def input "
###.#
..#..
#..#.
#....
.#.#.
")

(def test-input "
....#
#..#.
#..##
..#..
#....
")

(defn parse
  "Return map of coordinates, so that (get-in m [y x]) will return the element at that position"
  [s]
  (->> (string/split s #"\n")
       (filter seq)
       (map (partial zipmap (range)))
       (zipmap (range))))

(defn whats-at [m coordinates]
  {:pre [(#{2 3} (count coordinates))]}
  (if (= 2 (count coordinates))
    (let [[x y] coordinates]
      (get-in m [y x] \.))
    (let [[z x y] coordinates]
      (get-in m [z y x] \.))))

(defn render
  [m]
  (->> (for [y (range 5)
             x (range 5)] [x y])
       (map (partial whats-at m))
       (partition 5)
       (map (partial apply str))
       (string/join "\n")))

(defn adjacent [[x y]]
  #{[(inc x) y]
    [x (inc y)]
    [(dec x) y]
    [x (dec y)]})

(defn num-adjacent-bugs
  [m coordinates adjacency-fn]
  (->> (adjacency-fn coordinates)
       (map (partial whats-at m))
       (filter #(= \# %))
       count))

(defn minute-coordinates
  [m coordinates & {:keys [adjacency-fn] :or {adjacency-fn adjacent}}]
  (let [there (whats-at m coordinates)]
    (cond
      (and (= \# there)
           (not= 1 (num-adjacent-bugs m coordinates adjacency-fn))) \.
      (and (= \. there)
           (< 0 (num-adjacent-bugs m coordinates adjacency-fn) 3)) \#
      :else there)))

(defn minute [m]
  (->> (for [y (range 5)
             x (range 5)] [x y])
       (reduce (fn [new-m [x y]]
                 (assoc-in new-m [y x] (minute-coordinates m [x y])))
               {})))

(defn biodiversity-rating-coordinates
  [m [x y]]
  (let [there (whats-at m [x y])]
    (if (= there \#)
      (Math/pow 2 (+ (* y 5) x))
      0)))

(defn biodiversity-rating [m]
  (->> (for [y (range 5)
             x (range 5)] [x y])
       (map (partial biodiversity-rating-coordinates m))
       (apply +)
       int))

(defn biodiversity-rating-for-the-first-layout-that-appears-twice
  [m]
  (loop [m m
         seen #{m}]
    (let [nm (minute m)]
      (if (seen nm)
        (biodiversity-rating nm)
        (recur nm (conj seen nm))))))

;; Part 1

(let [m (parse test-input)]
  (biodiversity-rating-for-the-first-layout-that-appears-twice m))

(let [m (parse input)]
  (biodiversity-rating-for-the-first-layout-that-appears-twice m))

;; => 1113073

(defn parse-2
  "Return map of coordinates, so that (get-in m [y x]) will return the element at that position"
  [s]
  {0 (parse s)})

(defn render-2 [m]
  (map (fn [[k v]] [k (render v)]) m))

(defn outer-level
  [z [x y]]
  (cond
    (< x 0) [(dec z) 1 2]
    (< y 0) [(dec z) 2 1]
    (> x 4) [(dec z) 3 2]
    (> y 4) [(dec z) 2 3]
    :else [z x y]))

(defn adjacent-2 [[z x y]]
  (let [reg-adjacent (adjacent [x y])]
    (if (reg-adjacent [2 2])
      (let [level-z (->> (disj reg-adjacent [2 2])
                         (map (fn [[x y]] [z x y])))
            inner-level (inc z)]
        (cond
          (= x 1) (concat level-z (map (fn [y] [inner-level 0 y]) (range 5)))
          (= x 3) (concat level-z (map (fn [y] [inner-level 4 y]) (range 5)))
          (= y 1) (concat level-z (map (fn [x] [inner-level x 0]) (range 5)))
          (= y 3) (concat level-z (map (fn [x] [inner-level x 4]) (range 5)))))
      (map (partial outer-level z) reg-adjacent))))

(defn m->seq [m]
  (mapcat
   (fn [[z yxc]]
     (mapcat
      (fn [[y xc]]
        (map (fn [[x c]] [z x y c]) xc))
      yxc))
   m))

(defn minute-2 [m]
  (->> (m->seq m)
       ;; Get all the coordinates to update first
       (reduce (fn [s [z x y c]] (into s (adjacent-2 [z x y]))) #{})
       #_(remove (fn [[z x y]] (and (= x 2) (= y 2))))
       (reduce (fn [new-m [z x y]]
                 (assoc-in
                  new-m [z y x]
                  (minute-coordinates m [z x y] :adjacency-fn adjacent-2)))
               {})))

(defn dump [rendered]
  (println)
  (doseq [[k v] (sort-by first rendered)]
    (println k)
    (println v)))

(defn count-bugs
  [m]
  (->> (m->seq m)
       (filter (fn [[z x y c]] (= c \#)))
       count))

;; Render test input for checking

(let [m (parse-2 test-input)
      elapsed-mins 10]
  (->> m
       (iterate minute-2)
       (take (inc elapsed-mins))
       last
       render-2
       dump))

(let [m (parse-2 test-input)
      elapsed-mins 10]
  (->> m
       (iterate minute-2)
       (take (inc elapsed-mins))
       last
       count-bugs))

;; Part 2

(let [m (parse-2 input)
      elapsed-mins 200]
  (->> m
       (iterate minute-2)
       (take (inc elapsed-mins))
       last
       count-bugs))

;; => 1928
