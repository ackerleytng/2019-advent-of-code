(ns intcode-asteroids.core
  (:require [clojure.string :as s]))

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
  [ip instruction modes state]
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
        next-state
        (case instruction
          1 (assoc state (access state ip 2) (+ (access state ip 0 modes) (access state ip 1 modes)))
          2 (assoc state (access state ip 2) (* (access state ip 0 modes) (access state ip 1 modes)))
          3 (let [input (Integer/parseInt (read-line))]
              (assoc state (access state ip 0) input))
          4 (do
              (println (str "program: |" (access state ip 0 modes) "|"))
              state)
          5 state
          6 state
          7 (assoc state (access state ip 2) (if (< (access state ip 0 modes) (access state ip 1 modes))
                                               1 0))
          8 (assoc state (access state ip 2) (if (= (access state ip 0 modes) (access state ip 1 modes))
                                               1 0))
          99 state)]
    [next-state next-ip]))


(defn decode
  [ip state]
  (let [instruction-code (nth state ip)
        instruction (rem instruction-code 100)
        modes (quot instruction-code 100)]
    [modes instruction]))

(defn interpret [state ip]
  (let [[modes instruction] (decode ip state)
        [next-state next-ip] (evaluate ip instruction modes state)]
    (when next-ip
      (recur next-state next-ip))))

;; Read inputs

(def input-code
  (->> (s/split (slurp "./5-intcode-asteroids/input.txt") #",")
       (map #(Integer/parseInt (s/trim %)))
       vec))

;; Part 1 (input 1) and Part 2 (input 5)

(interpret input-code 0)

(comment
  (decode 0 [1002])
  (decode 0 [1101])
  (decode 0 [10003])

  (interpret [3,9,8,9,10,9,4,9,99,-1,8] 0)
  (interpret [3,9,7,9,10,9,4,9,99,-1,8] 0)
  (interpret [3,3,1108,-1,8,3,4,3,99] 0)
  (interpret [3,3,1107,-1,8,3,4,3,99] 0)
  (interpret [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] 0)
  (interpret [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] 0)
  (interpret [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
              1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
              999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] 0))
