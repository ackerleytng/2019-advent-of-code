(ns space-police.core
  (:require [clojure.string :as s]
            [quil.core :as q]))

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
      (= 2 (count next-outputs)) [:continue next-state remaining-inputs next-outputs]
      (nil? ip) [nil next-state remaining-inputs next-outputs]
      :else (recur next-state remaining-inputs next-outputs))))

;; Read inputs

(defn parse-input-code
  [filename]
  (->> (s/split (slurp filename) #",")
       (map #(read-string (s/trim %)))
       vec))

;; Part 1

(defn color
  [emergency-hull coordinates]
  (get-in emergency-hull coordinates 0))

(defn paint
  [emergency-hull coordinates color]
  (assoc-in emergency-hull coordinates color))

(def directions
  (array-map
   :north [0 1]
   :west [-1 0]
   :south [0 -1]
   :east [1 0]))

(defn execute-turn-and-step
  [coordinates direction turn]
  (let [base-directions
        (if (zero? turn) (keys directions)
            (reverse (keys directions)))
        next-direction
        (second
         (drop-while (complement #{direction})
                     (flatten (repeat 2 base-directions))))
        next-coordinates
        (vec (map + coordinates (directions next-direction)))]
    [next-coordinates next-direction]))

(defn robot-do
  [program-state coordinates direction emergency-hull]
  (let [[robot-should-continue next-program-state _ [color-to-paint turn]]
        (interpret program-state [(color emergency-hull coordinates)] [])]
    (if robot-should-continue
      (let [[next-coordinates next-direction]
            (execute-turn-and-step coordinates direction turn)
            next-emergency-hull
            (paint emergency-hull coordinates color-to-paint)]
        (recur next-program-state next-coordinates next-direction next-emergency-hull))
      emergency-hull)))

;; Part 1

(let [hull (robot-do
            {:memory {:imem (parse-input-code "./11-space-police/input.txt")}}
            [0 0]
            :north
            {})]
  (apply + (map (fn [[_ v]] (count v)) hull)))

;; => 2478

(defn painted-hull []
  (robot-do
   {:memory {:imem (parse-input-code "./11-space-police/input.txt")}}
   [0 0]
   :north
   {0 {0 1}}))

(defn ->color-markings
  "Turns a map of maps from the robot into a sequence of color markings
   ([color x y])"
  [map-of-maps]
  (mapcat (fn [[x m]] (map (fn [[y color]] [color x y]) m)) map-of-maps))

(defn draw []
  ;; make background black
  (q/background 0)
  ;; make points white
  (q/stroke 255)
  (q/stroke-weight 1)
  ;; move origin point to centre of the sketch
  ;; by default origin is in the left top corner
  (q/with-translation [(/ (q/width) 5) (/ (q/height) 2)]
    ;; parameter t goes 0, 0.01, 0.02, ..., 99.99, 100
    (doseq [[color x y] (->color-markings (painted-hull))]
      (when (= 1 color)
        (q/point x (- y))))))

;; run sketch
;; Didn't figure out how to enlarge pixels in quil
;; To read, open the svg and zoom in
(q/defsketch painted-hull
  :size [100 100]
  :renderer :svg
  :output-file "painted-hull.svg"
  :draw draw)

;; => HCZRUGAZ
