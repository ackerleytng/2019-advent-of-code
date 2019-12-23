(ns slam-shuffle.core
  (:require [clojure.string :as string]))

(defn deal
  [deck-size i-idx]
  (- (dec deck-size) i-idx))

()

(defn cut
  [deck-size n i-idx]
  (let [pos-forward (- deck-size n)
        n-idx (+ i-idx pos-forward)]
    (mod n-idx deck-size)))

(defn increment
  [deck-size n i-idx]
  (mod (* i-idx n) deck-size))

(defn parse [s]
  (->> (string/split s #"\n")
       (filter #(> (count %) 0))))

(defn interpret
  [deck-size i-idx command]
  (cond
    (= "deal into new stack" command)
    (deal deck-size i-idx)
    (.startsWith command "deal with increment")
    (increment deck-size (Integer/parseInt (re-find #"\d+" command)) i-idx)
    (.startsWith command "cut")
    (cut deck-size (Integer/parseInt (re-find #"-?\d+" command)) i-idx)))

(map
 #(let [s "
deal with increment 7
deal into new stack
deal into new stack
"]
    (reduce (partial interpret 10) % (parse s)))
 [0 3 6 9 2 5 8 1 4 7])

;; How to read the above:
;; What was at position 0 at the start (also card 0) is shuffled to position 0
;; What was at position 3 at the start (also card 3) is shuffled to position 1
;; What was at position 6 at the start (also card 3) is shuffled to position 2

;; => [0 3 6 9 2 5 8 1 4 7]
;; => (0 1 2 3 4 5 6 7 8 9)

(map
 #(let [s "
cut 6
deal with increment 7
deal into new stack
"]
    (reduce (partial interpret 10) % (parse s)))
 [3 0 7 4 1 8 5 2 9 6])

(map
 #(let [s "
deal with increment 7
deal with increment 9
cut -2
"]
    (reduce (partial interpret 10) % (parse s)))
 [6 3 0 7 4 1 8 5 2 9])

(map
 #(let [s "
deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
"]
    (reduce (partial interpret 10) % (parse s)))
 [9 2 5 8 1 4 7 0 3 6])

;; Part 1

;; The new position of card 2019 (also position 2019)

(reduce (partial interpret 10007) 2019 (parse (slurp "./22-slam-shuffle/input.txt")))

;; => 1510
