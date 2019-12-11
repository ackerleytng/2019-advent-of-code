(ns space-image.core
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(defn ->image
  [string]
  (map (fn [c] (- (int c) 48)) string))

(defn read-image
  [filename]
  (->image (slurp filename)))

;; Part 1

(let [layer-fewest-0s
      (->> (read-image "./8-space-image/input.txt")
           (partition (* 25 6))
           (apply min-key (fn [layer] (count (filter zero? layer)))))
      count-1s (count (filter #(= 1 %) layer-fewest-0s))
      count-2s (count (filter #(= 2 %) layer-fewest-0s))]
  (* count-1s count-2s))

;; => 1320

(defn render-pixel
  "Takes a sequence of pixels from top most pixel to bottom most"
  [& pixels]
  (first (drop-while #(= 2 %) pixels)))

;; Part 2

(->> (read-image "./8-space-image/input.txt")
     (partition (* 25 6))
     (apply map render-pixel)
     (partition 25)
     (map #(s/replace (apply str %) #"0" " "))
     (s/join "\n")
     println)

"111000110010001100101110010010100101000110100100101001010000010101100010010111001000000100101001110010100100100010010100101001001001100001001001010010"

(apply map render-pixel (->> (->image "0222112222120000")
                             (partition 4)))
