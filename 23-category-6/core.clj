(ns category-6.core
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]))

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
          (do (and (not= input -1) (println "input" input))
              (write memory (access-to-write state 0 modes) input))
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

(defn intcode-computer
  [{:keys [in-chan out-chan]} address]
  (a/go-loop [state {:memory {:imem (parse-input-code "./23-category-6/input.txt")}}
              outputs []
              addr address]
    (and (seq outputs) (println "outputs" outputs))
    (let [[_ instruction] (decode state)
          input (when (= 3 instruction)
                  (or addr
                      (first (a/alts! [in-chan] :default -1))))
          [{:keys [ip] :as next-state}
           next-outputs] (evaluate state input outputs)]
      (cond
        (= 3 (count next-outputs))
        (do
          (let [msg (vec next-outputs)]
            (a/>! out-chan msg)
            (println address "sent" next-outputs))
          (recur next-state [] nil))

        (nil? ip)
        (do
          (println "exited")
          next-outputs)

        :else
        (do
          (a/>! out-chan [200 addr 1234])
          (recur next-state next-outputs nil))))))

(defn close-all [channels]
  (doseq [{:keys [in-chan out-chan]} (vals channels)]
    (a/close! in-chan)
    (a/close! out-chan)))

(System/setProperty "clojure.core.async.go-checking" "true")
(System/getProperty "clojure.core.async.go-checking")

(comment

  (def channels
    (zipmap (range 50)
            (repeatedly (fn [] {:in-chan (a/chan 100) :out-chan (a/chan 100)}))))

  (do
    ;; Boot all computers
    (doseq [[addr {:keys [in-chan out-chan] :as inout}] channels]
      (intcode-computer inout addr))

    ;; Do routing
    (let [central (a/merge (mapv :out-chan (vals channels)))]
      (a/go-loop []
        (let [[addr x y] (a/<! central)]
          (cond
            (= addr 255)
            (do (close-all channels)
                (println "=================================>>>" y))

            (= addr 200)
            (recur)

            :else
            (do
              (println "routed" [x y] "to" addr)
              (let [c (get-in channels [addr :in-chan])]
                (a/>! c x)
                (a/>! c y))
              (recur)))))))

  )
