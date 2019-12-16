(ns oxygen-system.core
  (:require [clojure.string :as s]))

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

(defn read*
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
  (let [param (read* memory (+ ip (inc param-idx)))]
    (case (mode modes param-idx)
      0 param
      1 param
      2 (+ param relative-base))))

(defn access
  [{:keys [memory ip relative-base] :or {ip 0 relative-base 0}}
   param-idx modes]
  (let [param (read* memory (+ ip (inc param-idx)))]
    (case (mode modes param-idx)
      0 (read* memory param)
      1 param
      2 (read* memory (+ param relative-base)))))

(defn decode
  [{:keys [ip memory] :or {ip 0}}]
  (let [instruction-code (read* memory ip)
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
    (cond
      (nil? ip) next-outputs
      (seq next-outputs)
      [next-state remaining-inputs next-outputs]
      :else (recur next-state remaining-inputs next-outputs))))

(defn parse-input-code
  [filename]
  (->> (s/split (slurp filename) #",")
       (map #(read-string (s/trim %)))
       vec))

;; Approach
;; This is a graph search problem
;; We store the following information about every step
;; coordinates, program state for robot in those coordinates, status code for those coordinates, steps from origin

(defn step
  [coordinates direction]
  (let [delta ({1 [0 1] 2 [0 -1] 3 [-1 0] 4 [1 0]} direction)]
    (map + coordinates delta)))

(defn explore
  [{:keys [coordinates state]} direction]
  (let [next-coordinates (step coordinates direction)
        [next-state _ [status]] (interpret state [direction] [])]
    [{:coordinates next-coordinates
      :state next-state}
     status]))

;; Part 1
;; BFS

(def oxygen-system-state-steps
  (loop [visited #{[0 0]}
         work-list [{:coordinates [0 0]
                     :state {:memory {:imem (parse-input-code "./15-oxygen-system/input.txt")}}}]
         steps 0]
    (let [work (for [w work-list d (range 1 5)] [w d])
          results (map #(apply explore %) work)
          next-visited (apply conj visited (map (comp :coordinates first) results))
          next-work-list (->> results
                              (filter #(= 1 (second %)))
                              (map first)
                              (remove #(visited (:coordinates %))))
          found (filter #(= 2 (second %)) results)]
      (if (seq found)
        [(ffirst found) (inc steps)]
        (recur next-visited next-work-list (inc steps))))))

(second oxygen-system-state-steps)

;; => 366

;; Part 2
;; Still BFS
;; Let the oxygen explore instead, with the droid logic to guide it

(loop [visited #{(:coordinates (first oxygen-system-state-steps))}
       work-list [(first oxygen-system-state-steps)]
       minutes 0]
  (let [work (for [w work-list d (range 1 5)] [w d])
        results (map #(apply explore %) work)
        next-visited (apply conj visited (map (comp :coordinates first) results))
        next-work-list (->> results
                            (filter #(= 1 (second %)))
                            (map first)
                            (remove #(visited (:coordinates %))))]
    (if (seq next-work-list)
      (recur next-visited next-work-list (inc minutes))
      minutes)))
