(ns crossed-wires.core)

(defn has-two-same-adjacent-digits
  "Checks if this number has two adjacent digits that are identical
   Assumes that the number is positive"
  [number]
  (cond
    (< number 10) false
    (zero? (rem (rem number 100) 11)) true
    :else (recur (quot number 10))))

;; Exercising has-two-same-adjacent-digits

(has-two-same-adjacent-digits 1)
;; => false
(has-two-same-adjacent-digits 11)
;; => true
(has-two-same-adjacent-digits 121)
;; => false
(has-two-same-adjacent-digits 12234)
;; => true
(has-two-same-adjacent-digits 122244)
;; => true
(has-two-same-adjacent-digits 123456)
;; => false

(defn special-inc
  "Increments a number such that the resulting number
   always has digits that are increasing, for example,
   235778 increments to 235779
   235779 increments to 235788
   Assumes that the input number already has digits that are increasing"
  [number]
  (if (< (rem number 10) 9)
    (inc number)
    (let [inc-butlast (special-inc (quot number 10))]
      (+ (* inc-butlast 10) (rem inc-butlast 10)))))

;; Exercising special-inc

(take 10 (iterate special-inc 235777))

;; Part 1

;; The given range is 235741-706948
;; The first monotonically increasing number after 235741 is 235777

(->> (iterate special-inc 235777)
     (take-while #(< % 706948))
     (filter has-two-same-adjacent-digits)
     count)

;; => 1178

;; Part 2

(defn has-two-same-adjacent-digits-but-not-part-of-larger-group
  "Checks if this number has two adjacent digits that are identical,
   but not part of a larger group
   Assumes that the number is positive"
  ([number] (has-two-same-adjacent-digits-but-not-part-of-larger-group
             (quot number 10) (rem number 10) 1))
  ([number prev count]
   (cond
     (= prev (rem number 10)) (recur (quot number 10) prev (inc count))
     (= count 2) true
     (zero? (quot number 10)) false
     :else (recur (quot number 10) (rem number 10) 1))))

(map has-two-same-adjacent-digits-but-not-part-of-larger-group
     [112233 123444 111122 111223 112223 112222 122222 222222 110000 111000 110001])
;; => (true false true true true true false false true false true)

(->> (iterate special-inc 235777)
     (take-while #(< % 706948))
     (filter has-two-same-adjacent-digits-but-not-part-of-larger-group)
     count)

;; => 763
