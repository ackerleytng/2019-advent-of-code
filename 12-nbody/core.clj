(ns nbody.core
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(def start-velocities
  [[0 0 0]
   [0 0 0]
   [0 0 0]
   [0 0 0]])

(defn apply-gravity
  "Applies gravity, given positions p0, p1 (x, y or z) of two moons
   Returns the delta, to be applied on the respective velocities"
  [p0 p1]
  (cond
    (> p1 p0) [+1 -1]
    (> p0 p1) [-1 +1]
    :else [0 0]))

(defn apply-gravity-axes
  [a0 a1]
  (apply map vector (map apply-gravity a0 a1)))

(defn apply-gravity-moons
  [[i0 a0] [i1 a1]]
  (let [[vd0 vd1] (apply-gravity-axes a0 a1)]
    [[i0 vd0] [i1 vd1]]))

(defn velocity-delta
  [positions]
  (->> (combo/combinations (map vector (range) positions) 2)
       (mapcat (partial apply apply-gravity-moons))
       (group-by first)
       vals
       (map #(map second %))
       (map (partial apply map +))))

(defn step
  [{:keys [positions velocities]}]
  (let [delta (velocity-delta positions)
        next-velocities (map #(map + %1 %2) velocities delta)
        next-positions (map #(map + %1 %2) positions next-velocities)]
    {:positions next-positions :velocities next-velocities}))

(defn simulate
  [num-steps positions velocities]
  (->> {:positions positions :velocities velocities}
       (iterate step)
       (take (inc num-steps))
       last))

(defn energy
  [values]
  (map #(apply + (map (fn [v] (Math/abs v)) %)) values))

(defn energy-total
  [{:keys [positions velocities]}]
  (apply + (map * (energy positions) (energy velocities))))

(def input-positions
  [[16 -11 2]
   [0 -4 7]
   [6 4 -10]
   [-3 -2 -4]])

(def test-positions
  [[-1 0 2]
   [2 -10 -7]
   [4 -8 8]
   [3 5 -1]])

(def test-positions-2
  [[-8 -10 0]
   [5 5 10]
   [2 -7 3]
   [9 -8 -3]])

;; Part 1

(energy-total (simulate 1000 input-positions start-velocities))

;; => 10055

;; Part 2

(defn first-repeat
  "Brute-force way to find first repeat"
  [positions velocities]
  (let [beginning {:positions positions :velocities velocities}]
    (->> beginning
         (iterate step)
         (drop 1)
         (map-indexed (fn [i e] [i e]))
         (drop-while #(not= beginning (second %)))
         (take 1)
         ffirst
         inc)))

;; Works on test-positions

(first-repeat test-positions start-velocities)

;; Takes too long here

;; (first-repeat test-positions-2 start-velocities)

;; Part 2

;; Approach
;; Find cycle lengths for each axis, then find the lcm of the cycle lengths

(def test-x-positions
  [-1 2 4 3])

(defn apply-gravity-axis
  [[i0 p0] [i1 p1]]
  (let [[v0 v1] (apply-gravity p0 p1)]
    [[i0 v0] [i1 v1]]))

(defn velocity-delta-axis
  [axis-positions]
  (->> (combo/combinations (map vector [0 1 2 3] axis-positions) 2)
       (mapcat #(apply apply-gravity-axis %))
       (group-by first)
       vals
       (map #(apply + (map second %)))))

(defn step-axis
  [{:keys [positions velocities] :or {velocities [0 0 0 0]}}]
  (let [delta (velocity-delta-axis positions)
        next-velocities (map + delta velocities)
        next-positions (map + positions next-velocities)]
    {:positions next-positions :velocities next-velocities}))

(defn cycle-length
  [pos-vel]
  (loop [s (step-axis pos-vel)
         len 1]
    (if (= pos-vel s) len
        (recur (step-axis s) (inc len)))))

(comment

  (cycle-length {:positions test-x-positions :velocities [0 0 0 0]})

  )

(defn gcd
  [a b]
  (if (zero? b) a
      (recur b (mod a b))))

(defn- lcm*
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcm
  [& v]
  (reduce lcm* v))

(->> (apply map vector input-positions)
     (map (fn [pos] {:positions pos :velocities [0 0 0 0]}))
     (map cycle-length)
     (apply lcm))

;; => 374307970285176
