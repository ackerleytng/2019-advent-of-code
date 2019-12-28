(ns cryostasis.core
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.math.combinatorics :refer [subsets]]))

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
  [{:keys [ip relative-base memory] :as state :or {ip 0 relative-base 0}} input outputs]
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
          (write memory (access-to-write state 0 modes) input)
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
     next-outputs]))

(defn parse-input-code
  [filename]
  (->> (string/split (slurp filename) #",")
       (map #(read-string (string/trim %)))
       vec))

;; Part 1

(defn ascii->intcode
  [command]
  (map int (str command "\n")))

(defn intcode->ascii
  [intcode]
  (try
    (string/join
     (map char intcode))
    (catch IllegalArgumentException e
      intcode)))

(defn interpret
  [{:keys [ip] :or {ip 0} :as state} inputs outputs]
  (if (nil? ip)
    [:exited outputs]
    (let [[_ instruction] (decode state)]
      (cond
        (and (= instruction 3) (not (seq inputs)))
        [state outputs]

        :else
        (let [[next-state next-outputs] (evaluate state (first inputs) outputs)
              next-inputs (if (= instruction 3) (rest inputs) inputs)]
          (recur next-state next-inputs next-outputs))))))

(def start-state
  {:memory {:imem (parse-input-code "./25-cryostasis/input.txt")}})

(defn play [& {:keys [state] :or {state start-state}}]
  (loop [state state
         inputs []
         saves {}]
    (let [[next-state outputs] (interpret state inputs [])]
      (println (intcode->ascii outputs))
      (let [user-input (read-line)]
        (println ">>>" user-input)
        (cond
          (= user-input "Xquit") state
          (= :exited next-state) state
          :else
          (recur next-state (ascii->intcode user-input) saves))))))

(comment
  (spit "./25-cryostasis/collected-all-items-just-outside-portal-state.edn"
        (pr-str collected-all-items-just-outside-portal-state))
  )

(def collected-all-items-just-outside-portal-state
  (read-string (slurp "./25-cryostasis/collected-all-items-just-outside-portal-state.edn")))

(def items-collected-and-on-floor
  ["ornament"
   "klein bottle"
   "dark matter"
   "candy cane"
   "hologram"
   "astrolabe"
   "whirled peas"
   "tambourine"])

(defn try-items
  [state items]
  (let [take-commands (->> items
                           (map #(ascii->intcode (str "take " %)))
                           (apply concat))
        north-command (ascii->intcode "north")
        [state outputs] (interpret state (concat take-commands north-command) [])]
    {:state state
     :outputs outputs
     :items items}))

(defn summarize-output
  [{:keys [outputs] :as try-outcome}]
  (assoc
   try-outcome
   :summary
   (->> (intcode->ascii outputs)
        (re-find #"(heavier|lighter)"))))

(def all-try-outcomes
  (->> (subsets items-collected-and-on-floor)
       (map (comp summarize-output
                  (partial try-items collected-all-items-just-outside-portal-state)))
       (group-by :summary)))

;; Part 1

;; Get the outcome that neither matches heavier nor lighter

(->> (get all-try-outcomes nil)
     first
     :outputs
     intcode->ascii
     (re-find #"\d+")
     Integer/parseInt)

;; => 134349952

(comment

  (->> (try-items collected-all-items-just-outside-portal-state
                  (first (subsets items-collected-and-on-floor)))
       summarize-output)

  all-try-outcomes

  ( "klein bottle" "hologram" "astrolabe" "tambourine" )

  (play :state collected-all-items-just-outside-portal-state))
