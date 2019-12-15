(ns space-stoichiometry.core
  (:require [clojure.string :as s]))

(defn parse-quantity-chemical-pairs
  [string]
  (->> (re-seq #"(\d+) ([A-Z]+)" string)
       (map #(drop 1 %))
       (map (fn [[qty chemical]] [chemical (Integer/parseInt qty)]))
       (into {})))

(defn parse-reactions
  [input]
  (->> (s/split input #"\n")
       (filter (partial re-find #"=>"))
       (map #(s/split % #"=>"))
       (map (fn [[inputs outputs]]
              {:inputs (parse-quantity-chemical-pairs inputs)
               :output (parse-quantity-chemical-pairs outputs)}))))

;; Approach
;; Build graph of all reactions leading from ORE to FUEL
;; Using graph, build map of chemical to distance from ORE
;;   (ORE = 0, one hop from ORE = 1, ...)
;; Beginning with the reaction that produces FUEL,
;;   replace each input chemical with chemicals that output that
;;   replacing the chemical furthest from ORE first
;;   (replace derivatives before basic chemicals)

(defn reaction->edge
  [{:keys [inputs output]}]
  (let [output-chemical (first (keys output))]
    (map (fn [i] [i output-chemical]) (keys inputs))))

(defn reactions->graph
  [reactions]
  (->> reactions
       (mapcat reaction->edge)
       (group-by first)
       (map (fn [[k v]] [k (map second v)]))
       (into {})))

(defn graph->distance-counts
  [graph]
  (loop [work-list (graph "ORE")
         level 0
         counts {"ORE" 0}]
    (if (seq work-list)
      (recur
       (set (mapcat graph work-list))
       (inc level)
       (reduce (fn [c v] (assoc c v (inc level))) counts work-list))
      counts)))

(defn reactions->lookup-table
  [reactions]
  (->> reactions
       (group-by (comp first keys :output))
       (map (fn [[k v]] [k (first v)]))
       (into {})))

(defn substituted-inputs
  [qty {:keys [inputs output]}]
  (let [min-qty (first (vals output))
        multiplier
        (quot (+ qty (dec min-qty)) min-qty)]
    (->> inputs
         (map (fn [[k v]] [k (* multiplier v)]))
         (into {}))))

(comment

  (substituted-inputs 3 {:inputs {"A" 1 "B" 1} :output {"C" 3}})

  )

(defn substitute-chemical
  [distance-counts lookup inputs]
  (let [sorted-inputs (reverse (sort-by (comp distance-counts first) inputs))
        [chemical qty] (first sorted-inputs)
        reaction (lookup chemical)
        new-inputs (substituted-inputs qty reaction)
        remaining-inputs (->> sorted-inputs
                              (drop 1)
                              (into {}))]
    (merge-with + remaining-inputs new-inputs)))

(defn ore-requirement*
  [lookup distance-counts inputs]
  (loop [inputs inputs]
    (if (and (= 1 (count inputs)) (= "ORE" (ffirst inputs)))
      (first (vals inputs))
      (recur
       (substitute-chemical
        distance-counts
        lookup
        inputs)))))

(defn ore-requirement
  [reactions]
  (let [lookup (reactions->lookup-table reactions)
        distance-counts (graph->distance-counts (reactions->graph reactions))]
    (ore-requirement* lookup distance-counts (:inputs (lookup "FUEL")))))


(def rxn-0
  "
10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
")

(def rxn-1
  "
9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL
")

(def rxn-2
  "
157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
")

(def rxn-3
  "
2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF
")

(def rxn-4
  "
171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX
")

;; Part 1

(->> (parse-reactions (slurp "./14-space-stoichiometry/input.txt"))
     ore-requirement)

;; => 741927

;; Part 2

;; Approach
;; Treat FUEL as an input for something bigger
;; Use ore-requirement* to compute ORE requirement,
;;   check that it is less than cargo-hold-ore-qty
;; Find biggest FUEL qty that is still less than cargo-hold-ore-qty
;; Do this because ore-requirement* helps us to re-use
;;   extra intermediate products for the next FUEL
;; We can search beginning with (quot cargo-hold-ore-qty <requirement for 1 FUEL>)
;;   because we can only do better than that number, not worse

(def cargo-hold-ore-qty
  1000000000000)

(defn can-generate)

(defn max-possible-fuel-production
  [reactions]
  (let [base-requirement (ore-requirement reactions)
        min-fuel-production (quot cargo-hold-ore-qty base-requirement)
        lookup (reactions->lookup-table reactions)
        distance-counts (graph->distance-counts (reactions->graph reactions))]
    (loop [try-value min-fuel-production
           last-tried nil]
      (let [requirement (ore-requirement* lookup distance-counts {"FUEL" try-value})
            ore-to-go (- cargo-hold-ore-qty requirement)
            jump-qty (quot ore-to-go base-requirement)]
        (cond
          (and (zero? jump-qty) (neg? ore-to-go))
          last-tried
          (zero? jump-qty) (recur (inc try-value) try-value)
          :else (recur (+ try-value jump-qty) try-value))))))

(max-possible-fuel-production
 (parse-reactions (slurp "./14-space-stoichiometry/input.txt")))

;; => 2371699
