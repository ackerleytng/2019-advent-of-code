(ns slam-shuffle.core
  (:require [clojure.string :as string]))

(defn deal
  [deck-size i-idx]
  (- deck-size i-idx 1))

(def undo-deal deal)

(defn cut
  [deck-size n i-idx]
  (let [pos-forward (- deck-size n)
        n-idx (+ i-idx pos-forward)]
    (mod n-idx deck-size)))

(defn undo-cut
  [deck-size n o-idx]
  (mod (+ o-idx n) deck-size))

(defn increment
  [deck-size n i-idx]
  (mod (* i-idx n) deck-size))

(defn undo-increment
  [deck-size n o-idx]
  (/ (->> (map #(+ (* % deck-size) o-idx) (range))
          (filter #(zero? (mod % n)))
          first) n))

(map (partial increment 10 3) (range 10))

(map (partial undo-increment 10 3)
     (vector 0 3 6 9 2 5 8 1 4 7))

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

(defn undo-interpret
  [deck-size o-idx command]
  (cond
    (= "deal into new stack" command)
    (undo-deal deck-size o-idx)
    (.startsWith command "deal with increment")
    (undo-increment deck-size (Integer/parseInt (re-find #"\d+" command)) o-idx)
    (.startsWith command "cut")
    (undo-cut deck-size (Integer/parseInt (re-find #"-?\d+" command)) o-idx)))

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

;; => inputs  [0 3 6 9 2 5 8 1 4 7]
;; => outputs (0 1 2 3 4 5 6 7 8 9)

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

;; Tried undo functions, but they were too slow

(map
 #(let [s "
deal with increment 7
deal into new stack
deal into new stack
"]
    (reduce (partial undo-interpret 10) % (reverse (parse s))))
 (range 10))

;; How to read the above:
;; What was at position 0 at the end was shuffled from position 0
;; What was at position 1 at the end was shuffled from position 3
;; What was at position 2 at the end was shuffled from position 6
;; => inputs  (0 1 2 3 4 5 6 7 8 9)
;; => outputs [0 3 6 9 2 5 8 1 4 7]

(map
 #(let [s "
cut 6
deal with increment 7
deal into new stack
"]
    (reduce (partial undo-interpret 10) % (reverse (parse s))))
 (range 10))

;; => [3 0 7 4 1 8 5 2 9 6]

(map
 #(let [s "
deal with increment 7
deal with increment 9
cut -2
"]
    (reduce (partial undo-interpret 10) % (reverse (parse s))))
 (range 10))

;; => [6 3 0 7 4 1 8 5 2 9]

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
    (reduce (partial undo-interpret 10) % (reverse (parse s))))
 (range 10))

;; => [9 2 5 8 1 4 7 0 3 6]

;; Too slow

#_
(reduce (partial undo-interpret 119315717514047) 2020
        (apply concat (take 101741582076661
                            (repeat (reverse (parse (slurp "./22-slam-shuffle/input.txt")))))))

;; Learnt the approach from
;; https://github.com/rabuf/advent-of-code/blob/master/2019/2019.22.org

;; 1. Model the forward function as (mod (+ (* A i-idx) B) deck-size)
;; 2. Run part 1 code (forward) to find out parameters A and B for just 1 iteration
;;    of the input commands
;; 3. Mathematically compute the function for the full number of iterations
;; 4. Invert the function found in 3, apply that on 2020

;; Run part 1 code forward for i-idx = 0

(def deck-size-2 119315717514047)
(def shuffle-times 101741582076661)

(def B
  (reduce (partial interpret deck-size-2)
          0 (parse (slurp "./22-slam-shuffle/input.txt"))))

;; => 18613911920679

;; For the function (mod (+ (* A i-idx) B) deck-size),
;; Since i-idx = 0, (as used above) we know that B = 18613911920679

(def A-plus-B
  (reduce (partial interpret deck-size-2)
          1 (parse (slurp "./22-slam-shuffle/input.txt"))))

;; For the function (mod (+ (* A i-idx) B) deck-size),
;; Since i-idx = 1, (as used above) we know that A + B = 78726802031173

(def A
  (- A-plus-B B))

;; Shuffling once would be
;; S(i) = (Ai + B) mod D
;; where S is shuffling the deck using just 1 iteration of the input commands
;;       i is the input index (i-idx in other functions)
;;       D is deck-size

;; We take out the mod D part to simplify the math since
;; (x * y) mod D = ((x mod D) * (y mod D)) mod D
;; and
;; (x + y) mod D = ((x mod D) + (y mod D)) mod D

;; Exploring this,
;; S(S(i)) = A(Ai + B) + B
;;         = A^2 * i + AB + B

;; S^m(i) = A^m * i + sum(A^n * B) for n = 0 to m - 1
;;        = A^m * i + B * sum(A^n) for n = 0 to m - 1
;;                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                        ^ geometric series
;;        = A^m * i + B * (A^m - 1) / (A - 1)
;; where m is the number of times the shuffling sequence is repeated

;; Invert that function (let S^m(i) = k)

;; invert(k) = (k - B * (A^m - 1) * modinv(A - 1, D)) * modinv(A, m)
;;                                                                   (mod D)
;; Modular arithmetic has no division operation;
;;   modinv(A, m) is called a modular inverse, defined as
;;   (A * A^-1) mod C = 1

;; Since m is prime, the modular inverse is given by
;;   A^(m-2) (mod m)

(defn modinv-prime
  "Returns the modular inverse for prime powers"
  [base power]
  (.modPow (biginteger base) (biginteger (- power 2)) (biginteger power)))

(defn invert [k]
  (let [modinv-A (modinv-prime A deck-size-2)
        modinv-A-m (.modPow modinv-A (biginteger shuffle-times) (biginteger deck-size-2))
        modinv-A-minus-1-D (modinv-prime (dec A) deck-size-2)]
    (mod (- (* k modinv-A-m)
            (* B (- 1 modinv-A-m) modinv-A-minus-1-D))
         deck-size-2)))

;; Part 2

(invert 2020)

;; => 10307144922975N
