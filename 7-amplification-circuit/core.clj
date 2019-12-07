(ns amplification-circuit.core
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(defn mode [modes param-idx]
  (if (zero? param-idx)
    (rem modes 10)
    (recur (quot modes 10) (dec param-idx))))

(defn access
  ;; Either mode
  ([state ip param-idx modes]
   (let [param (nth state (+ ip (inc param-idx)))]
     (case (mode modes param-idx)
       0 (nth state param)
       1 param)))
  ;; Position mode
  ([state ip param-idx]
   (nth state (+ ip (inc param-idx)))))

(defn evaluate
  [ip instruction modes state inputs outputs]
  (let [next-ip
        (cond
          (#{1 2 7 8} instruction) (+ ip (if (= (access state ip 2) ip) 0 4))

          (= 3 instruction) (+ ip (if (= (access state ip 0) ip) 0 2))
          (= 4 instruction) (+ ip 2)
          (= 5 instruction) (if (not= (access state ip 0 modes) 0)
                              (access state ip 1 modes)
                              (+ ip 3))
          (= 6 instruction) (if (= (access state ip 0 modes) 0)
                              (access state ip 1 modes)
                              (+ ip 3))
          (= 99 instruction) nil)
        remaining-inputs
        (cond
          (#{1 2 4 5 6 7 8 99} instruction) inputs
          (= 3 instruction) (vec (rest inputs)))
        next-outputs
        (cond
          (#{1 2 3 5 6 7 8 99} instruction) outputs
          (= 4 instruction) (conj outputs (access state ip 0 modes)))
        next-state
        (case instruction
          1 (assoc state (access state ip 2) (+ (access state ip 0 modes) (access state ip 1 modes)))
          2 (assoc state (access state ip 2) (* (access state ip 0 modes) (access state ip 1 modes)))
          3 (assoc state (access state ip 0) (first inputs))
          4 state
          5 state
          6 state
          7 (assoc state (access state ip 2) (if (< (access state ip 0 modes) (access state ip 1 modes))
                                               1 0))
          8 (assoc state (access state ip 2) (if (= (access state ip 0 modes) (access state ip 1 modes))
                                               1 0))
          99 state)]
    [next-state next-ip remaining-inputs next-outputs]))


(defn decode
  [ip state]
  (let [instruction-code (nth state ip)
        instruction (rem instruction-code 100)
        modes (quot instruction-code 100)]
    [modes instruction]))

(defn interpret
  ([state inputs outputs] (interpret state 0 inputs outputs))
  ([state ip inputs outputs]
   (let [[modes instruction] (decode ip state)
         [next-state next-ip remaining-inputs next-outputs]
         (evaluate ip instruction modes state inputs outputs)]
     (if (or (seq next-outputs) (nil? next-ip))
       [next-state next-ip remaining-inputs next-outputs]
       (recur next-state next-ip remaining-inputs next-outputs)))))

;; Read inputs

(defn parse-input-code
  [filename]
  (->> (s/split (slurp filename) #",")
       (map #(Integer/parseInt (s/trim %)))
       vec))

;; Part 1

(defn run-amplify-program [program phase-setting]
  (loop [input 0
         setting phase-setting]
    (if-let [s (first setting)]
      (let [[_ _ _ outputs] (interpret program [s input] [])]
        (recur (first outputs) (rest setting)))
      input)))

(comment
  (run-amplify-program [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
                       [4,3,2,1,0])

  (run-amplify-program [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
                       [0,1,2,3,4])

  (run-amplify-program [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,
                        33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
                       [1,0,4,3,2])
  )

(->> (combo/permutations (range 5))
     (map #(run-amplify-program (parse-input-code "./7-amplification-circuit/input.txt") %))
     (apply max))

;; => 17790

;; Part 2

(defn- amplify-loop-program
  [amplifier-idx
   amplifier-states
   amplifier-inputs
   output]
  (let [[state ip] (nth amplifier-states amplifier-idx)
        [saved-state saved-ip remaining-inputs [result]]
        (interpret state ip (conj (nth amplifier-inputs amplifier-idx) output) [])]
    (if saved-ip
      (recur
       (rem (inc amplifier-idx) 5)
       (assoc amplifier-states amplifier-idx [saved-state saved-ip])
       (assoc amplifier-inputs amplifier-idx remaining-inputs)
       result)
      output)))

(defn run-amplify-loop-program
  [program phase-setting]
  (amplify-loop-program
   0
   (vec (take 5 (repeat [program 0])))
   (vec (map (fn [s] [s]) phase-setting))
   0))

(comment
  (run-amplify-loop-program
   [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
   [9,8,7,6,5])

  (run-amplify-loop-program
   [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
    -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
    53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
   [9,7,8,5,6])
  )

(->> (combo/permutations (range 5 10))
     (map #(run-amplify-loop-program (parse-input-code "./7-amplification-circuit/input.txt") %))
     (apply max))

;; => 19384820
