(ns sensor-boost.core
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(defn mode [modes param-idx]
  (if (zero? param-idx)
    (rem modes 10)
    (recur (quot modes 10) (dec param-idx))))

;; state is a map of
;; :memory = a map of
;;   :imem = instruction memory. vector of integers
;;   :dmem = map of address to value (using a map in case the storage is really sparse)
;; :ip = instruction pointer. integer
;; :relative-base = integer

(defn read
  [{:keys [imem dmem] :or {dmem {}}} addr]
  (if (< addr (count imem))
    (nth imem addr)
    (get dmem addr 0)))

(defn write
  [{:keys [imem] :as state} addr value]
  (if (< addr (count imem))
    (assoc-in state [:imem addr] value)
    (assoc-in state [:dmem addr] value)))

(defn access-to-write
  [{:keys [memory ip relative-base] :or {ip 0 relative-base 0}}
   param-idx modes]
  (let [param (read memory (+ ip (inc param-idx)))]
    (case (mode modes param-idx)
      0 param
      1 param
      2 (+ param relative-base))))

(defn access
  [{:keys [memory ip relative-base] :or {ip 0 relative-base 0}}
   param-idx modes]
  (let [param (read memory (+ ip (inc param-idx)))]
    (case (mode modes param-idx)
      0 (read memory param)
      1 param
      2 (read memory (+ param relative-base)))))

(defn decode
  [{:keys [ip memory] :or {ip 0}}]
  (let [instruction-code (read memory ip)
        instruction (rem instruction-code 100)
        modes (quot instruction-code 100)]
    [modes instruction]))

;; 1 = add
;; 2 = multiply
;; 3 = input
;; 4 = output
;; 5 = jnz
;; 6 = jz
;; 7 = set if less than
;; 8 = set if equals
;; 9 = adjust relative base

(defn evaluate
  [{:keys [ip relative-base memory] :as state :or {ip 0 relative-base 0}} inputs outputs]
  (let [[modes instruction] (decode state)
        next-ip
        (cond
          (#{1 2 7 8} instruction) (+ ip (if (= (access-to-write state 2 modes) ip) 0 4))

          (= 3 instruction) (+ ip (if (= (access-to-write state 0 modes) ip) 0 2))
          (#{4 9} instruction) (+ ip 2)
          (= 5 instruction) (if (not= (access state 0 modes) 0)
                              (access state 1 modes)
                              (+ ip 3))
          (= 6 instruction) (if (= (access state 0 modes) 0)
                              (access state 1 modes)
                              (+ ip 3))
          (= 99 instruction) nil)
        remaining-inputs
        (cond
          (#{1 2 4 5 6 7 8 9 99} instruction) inputs
          (= 3 instruction) (vec (rest inputs)))
        next-relative-base
        (cond
          (#{1 2 3 4 5 6 7 8 99} instruction) relative-base
          (= 9 instruction) (+ relative-base (access state 0 modes))) ;; TODO
        next-outputs
        (cond
          (#{1 2 3 5 6 7 8 9 99} instruction) outputs
          (= 4 instruction) (conj outputs (access state 0 modes)))
        next-memory
        (cond
          (= 1 instruction)
          (write memory (access-to-write state 2 modes)
                 (+ (access state 0 modes) (access state 1 modes)))
          (= 2 instruction)
          (write memory (access-to-write state 2 modes)
                 (* (access state 0 modes) (access state 1 modes)))
          (= 3 instruction)
          (write memory (access-to-write state 0 modes)
                 (first inputs))
          (#{4 5 6 9 99} instruction)
          memory
          (= 7 instruction)
          (write memory (access-to-write state 2 modes)
                 (if (< (access state 0 modes) (access state 1 modes)) 1 0))
          (= 8 instruction)
          (write memory (access-to-write state 2 modes)
                 (if (= (access state 0 modes) (access state 1 modes)) 1 0)))]
    [{:memory next-memory
      :ip next-ip
      :relative-base next-relative-base}
     remaining-inputs next-outputs]))

(defn interpret
  [state inputs outputs]
  (let [[{:keys [ip] :as next-state} remaining-inputs next-outputs]
        (evaluate state inputs outputs)]
    (if (nil? ip) next-outputs
        (recur next-state remaining-inputs next-outputs))))

(comment
  ;; should output itself
  (let [code [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
    (= (interpret {:memory {:imem code}} [] [])
       code))

  ;; should output a 16 digit number
  (->> (interpret {:memory {:imem [1102,34915192,34915192,7,4,7,99,0]}} [] [])
       first
       str
       count)

  ;; should output 1125899906842624
  (interpret {:memory {:imem [104,1125899906842624,99]}} [] [])
  )

;; Read inputs

(defn parse-input-code
  [filename]
  (->> (s/split (slurp filename) #",")
       (map #(Integer/parseInt (s/trim %)))
       vec))

;; Part 1

(->> (interpret {:memory {:imem (parse-input-code "./9-sensor-boost/input.txt")}} [1] [])
     first)

;; => 3989758265

;; Part 2

(->> (interpret {:memory {:imem (parse-input-code "./9-sensor-boost/input.txt")}} [2] [])
     first)

;; => 76791
