(ns clojure-app.part2.pair
  (:require [clojure-app.part1.math :as math]))

; Exercise 2.5
;
; Represents pair of values 'a' and 'b' as a product of 2^a * 3^b
;
;(defn cons [a b]
;  [(* (math/fast-exp 2 a) (math/fast-exp 3 b))]
;  )
;
;(defn car [pair]
;
;  (defn car-it [value count]
;    (if (even? value)
;      (car-it (/ value 2) (+ count 1))
;      count
;      )
;    )
;
;  (car-it (first pair) 0)
;
;  )
;
;(defn cdr [pair]
;
;  (defn remove-twos [value]
;    (if (odd? value)
;      value
;      (remove-twos (/ value 2))
;      )
;    )
;
;  (defn count-threes [value count]
;    (if (= value 1)
;      count
;      (count-threes (/ value 3) (+ count 1))
;      )
;    )
;
;  (count-threes (remove-twos (first pair)) 0)
;  )


; Exercise 2.4.
;(defn cons [x y]
;  (fn [m]
;    (m x y)
;    )
;  )
;
;(defn car [z]
;  (z (fn [p q] p))
;  )
;
;(defn cdr [z]
;  (z (fn [p q] q))
;  )

;
; Normal pair
(defn cons [first second]

  (fn [method]

    (cond
      (= method "first") first
      (= method "second") second
      :else nil
      )
    )
  )

(defn car [pair]
  (pair "first")
  )

(defn cdr [pair]
  (pair "second")
  )
