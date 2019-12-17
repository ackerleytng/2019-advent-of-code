(ns fft.core
  (:require [clojure.string :as s]))

(defn ->signal
  [string]
  (map (fn [c] (- (int c) (int \0))) string))

(defn phase
  [idx]
  (drop 1 (apply interleave (repeat idx (cycle [0 1 0 -1])))))

(defn output
  [signal idx]
  (mod (Math/abs (apply + (map * signal (phase (inc idx))))) 10))

(defn transform
  [signal]
  (let [n (count signal)]
    (map #(output signal %) (range n))))

(defn run
  [signal phases]
  (->> signal
       (iterate transform)
       (drop phases)
       first))

(take 8 (run (->signal "80871224585914546619083218645595") 100))

(take 8 (run (->signal "19617804207202209144916044189917") 100))

(take 8 (run (->signal "69317163492948606335995924319873") 100))

;; Part 1

(def puzzle-signal
  "59766299734185935790261115703620877190381824215209853207763194576128635631359682876612079355215350473577604721555728904226669021629637829323357312523389374096761677612847270499668370808171197765497511969240451494864028712045794776711862275853405465401181390418728996646794501739600928008413106803610665694684578514524327181348469613507611935604098625200707607292339397162640547668982092343405011530889030486280541249694798815457170337648425355693137656149891119757374882957464941514691345812606515925579852852837849497598111512841599959586200247265784368476772959711497363250758706490540128635133116613480058848821257395084976935351858829607105310340")

(take 8 (run (->signal puzzle-signal) 100))

;; => (1 8 9 3 3 3 6 4)

;; Part 2

;; Approach
;; Since the offset is somewhere in the second half of the full series of numbers
;;   (input length is 320000; offset is 303673)
;; We don't actually have to compute the all the earlier numbers
;; Looking closely at the second half of the full series,
;;   the last possible number is the last number;
;;   the second last number is the sum of the last number and the second last (mod 10);
;;   this goes on within the entire second half
;; This also happens for every iteration (the rest of the [0 1 0 -1] pattern to be multiplied
;;   are just zeroes, and this happens in every of the 100 iterations

(defn step-second-half
  [series-length signal]
  (loop [output []
         sum 0
         remaining-length series-length
         signal-backward (cycle (reverse signal))]
    (if (zero? remaining-length)
      output
      (let [next-sum (+ sum (first signal-backward))]
        (recur
         (cons (mod next-sum 10) output)
         next-sum
         (dec remaining-length)
         (drop 1 signal-backward))))))

(defn offset
  [string]
  (Integer/parseInt (s/join (take 7 string))))

(defn run-second-half
  [signal phases series-length]
  (->> signal
       (iterate (partial step-second-half series-length))
       (drop phases)
       first))

(defn message
  [string]
  (let [raw string
        length (* (count raw) 10000)
        o (offset raw)]
    (assert (> o (/ length 2)))
    (->> (run-second-half (->signal raw) 100 (- length o))
         (take 8))))

(message "03036732577212944063491565474664")

(message "02935109699940807407585447034323")

(message "03081770884921959731165446850517")

(message puzzle-signal)

;; => (2 8 8 7 2 3 0 5)
