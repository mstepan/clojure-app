(ns clojure-app.part1.find-roots
  (:require [clojure-app.part1.math :as math]))

(defn close-enough? [a b]
  (< (math/abs (- a b)) 0.01)
  )

(defn positive? [val]
  (> val 0)
  )

(defn negative? [val]
  (< val 0)
  )

; Use bisection method to find roots of a function.
; f(x) = 0
; time: O(lgN)
; space: O(1)
(defn bisection [f left right]

  (let [mid (math/average left right)]
    (if (close-enough? left right)
      mid

      (let [mid-value (f mid)]
        (cond
          (negative? mid-value) (bisection f mid right)
          (positive? mid-value) (bisection f left mid)
          ; 3rd case mid-val is '0'
          :else mid
          )
        )

      )
    )
  )


(defn search-for-root [f a b]

  (let [a-val (f a)
        b-val (f b)]
    (cond
      (and (negative? a-val) (positive? b-val)) (bisection f a b)
      (and (negative? b-val) (positive? a-val)) (bisection f b a)
      :else (println
              (str "both values are negative/positive: f(" a ") = " a-val
                   ", f(" b ") = " b-val))
      )
    )

  )
