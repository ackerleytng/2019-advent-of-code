(ns set-forget.core
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
      :else (recur next-state remaining-inputs next-outputs))))

(defn parse-input-code
  [filename]
  (->> (s/split (slurp filename) #",")
       (map #(read-string (s/trim %)))
       vec))

;; Part 1

(defn idx->coordinates
  [{:keys [width]} idx]
  (let [w (inc width)]
    [(mod idx w) (quot idx w)]))

(defn coordinates->idx
  [{:keys [width]} [x y]]
  (+ (* y (inc width)) x))

(defn width
  "Returns the width of the view, excluding the \n on the right"
  [raw-view]
  (.indexOf raw-view 10))

(defn height
  "Returns the height of the view"
  [raw-view]
  (let [w (width raw-view)]
    (quot (dec (count raw-view)) w)))

;; A view is actually
;; :view = the actual view
;; :height = height of the view
;; :width = width of the view

(def part-1-camera-view
  (let [raw-view (interpret {:memory {:imem (parse-input-code "./17-set-forget/input.txt")}} [] [])]
    {:view raw-view
     :height (height raw-view)
     :width (width raw-view)}))

(defn render-view
  [{:keys [view]}]
  (println (->> view
                (map char)
                (s/join))))

(render-view part-1-camera-view)

(defn inspect-pixel
  [{:keys [view width] :as v} coordinates]
  (nth view (coordinates->idx v coordinates)))

(map (partial inspect-pixel part-1-camera-view) [[28 0] [29 0] [28 1] [27 1]])

;; BFS over the scaffolding, save all intersections
;; intersection => up, down, left, right are all scaffolding

(defn find-scaffolding
  "Find any scaffolding in view"
  [{:keys [view] :as v}]
  (let [idx (.indexOf view 35)]
    (idx->coordinates v idx)))

(find-scaffolding part-1-camera-view)

(defn within-view
  [{:keys [width height]} [x y]]
  (and (<= 0 x (dec width)) (<= 0 y (dec height))))

(defn neighbors
  "Return all the neighbors within the view of the camera"
  [view coordinates]
  (filter (partial within-view view)
          (map #(map + coordinates %) [[0 1] [0 -1] [1 0] [-1 0]])))

(neighbors part-1-camera-view [0 0])
(neighbors part-1-camera-view [50 32])

(defn scaffolding?
  [view coordinates]
  (= 35 (inspect-pixel view coordinates)))

(defn intersection?
  [view coordinates]
  (let [nbrs (neighbors view coordinates)]
    (and (= 4 (count nbrs))
         (every? (partial scaffolding? view) nbrs))))

(defn intersections
  [view]
  (let [start (find-scaffolding view)]
    (loop [visited #{start}
           work-list (->> (neighbors view start)
                          (filter (partial scaffolding? view)))
           output #{}]
      (if (seq work-list)
        (recur (apply conj visited work-list)
               (->> work-list
                    (mapcat (partial neighbors view))
                    (filter (partial scaffolding? view))
                    (remove visited))
               (apply conj output (filter (partial intersection? view) work-list)))
        output))))

;; Part 1

(apply + (map (fn [[x y]] (* x y)) (intersections part-1-camera-view)))

;; => 7816

;; Part 2

(defn patch-vacuum-robot-awake
  [code]
  (assoc code 0 2))

(defn check-assemble-program
  [main-routine fn-a fn-b fn-c video-feed?]
  {:pre [(< (count main-routine) 20)
         (< (count fn-a) 20)
         (< (count fn-b) 20)
         (< (count fn-c) 20)
         (or (= "y" video-feed?)
             (= "n" video-feed?))]}
  (map int (str main-routine "\n"
                fn-a "\n"
                fn-b "\n"
                fn-c "\n"
                video-feed? "\n")))

(comment
  "Manually trace the route the robot should take:"

  "R,10,L,12,R,6,R,10,L,12,R,6,R,6,R,10,R,12,R,6,R,10,L,12,L,12,R,6,R,10,R,12,R,6,R,10,L,12,L,12,R,6,R,10,R,12,R,6,R,10,L,12,L,12,R,6,R,10,R,12,R,6,R,10,L,12,R,6"

  "Split it into functions:"

  (set ["R,10,L,12,R,6,"
        "R,10,L,12,R,6,"
        "R,6,R,10,R,12,R,6,"
        "R,10,L,12,L,12,"
        "R,6,R,10,R,12,R,6,"
        "R,10,L,12,L,12,"
        "R,6,R,10,R,12,R,6,"
        "R,10,L,12,L,12,"
        "R,6,R,10,R,12,R,6,"
        "R,10,L,12,R,6,"])

  )

(def part-2-output
  (interpret {:memory {:imem (patch-vacuum-robot-awake (parse-input-code "./17-set-forget/input.txt"))}}
             (check-assemble-program
              "A,A,B,C,B,C,B,C,B,A"
              "R,10,L,12,R,6"
              "R,6,R,10,R,12,R,6"
              "R,10,L,12,L,12"
              "n")
             []))

(render-view (let [raw-view (drop-last part-2-output)]
               {:view raw-view
                :height (height raw-view)
                :width (width raw-view)}))

(last part-2-output)

;; => 952010
