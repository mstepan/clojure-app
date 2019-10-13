(ns clojure-app.part1.sums
  (:require [clojure-app.part1.math :as math])
  (:require [clojure-app.part1.prime :as prime]))


; Higher-order function to calculate sequence with filtering predicate.
; time: O(N), where N = b-a
; space: O(1), use iterative recursion
(defn filtered-accumulator [combiner initial-value a b term next
                            filter-predicate]
  ((fn accumulate-it [x y res]
     (if (> x y)
       res

       (if (filter-predicate x)
         (accumulate-it (next x) y (combiner res (term x)))
         (accumulate-it (next x) y res)
         )
       )
     ) a b initial-value)
  )

(defn accumulate [combiner initial-value a b term next]

  (defn always-true-predicate [value]
    true
    )

  (filtered-accumulator combiner initial-value a b term next (fn [_] true))

  )

; Higher-order function to calculate 'product' of sequence.
(defn product [a b term next]
  (accumulate (fn [x y] (* x y)) 1 a b term next)
  )

; Higher-order function to calculate 'sum' of sequence.
(defn sum [a b term next]
  (accumulate (fn [x y] (+ x y)) 0 a b term next)
  )

(defn cube [a]
  (* a a a)
  )

(defn sum-integers [a b]
  (sum a b identity inc)
  )

(defn prod-integers [a b]
  (product a b identity inc)
  )

(defn sum-cubes [a b]
  (sum a b cube inc)
  )

(defn pi-sum [a b]
  (sum a b
       (fn [x] (/ 1.0 (* x (+ x 2))))
       (fn [x] (+ x 4)))
  )

(defn pi-approximation []
  (* 8 (pi-sum 1 1000))
  )

(defn integral [f a b dx]
  (* (sum (+ a (/ dx 2.0)) b f (fn [x] (+ x dx))) dx)
  )

; 1.33.a
;the sum of the squares of the prime numbers in the interval a to b
(defn sum-of-prime-squares [a b]
  (filtered-accumulator (fn [x y] (+ x y))
                        0
                        a
                        b
                        (fn [x] (* x x))
                        inc
                        prime/check-prime?)
  )


; 1.33.b
;the product of all the positive integers less than n that
;are relatively prime to n (i.e., all positive integers i < n
;such that gcd(i, n) = 1)
(defn product-of-relative-primes [val]

  (defn relative-prime? [other]
    (= (math/gcd val other) 1)
    )

  (filtered-accumulator (fn [x y] (* x y))
                        1
                        1
                        (- val 1)
                        identity
                        inc
                        (fn [other] (= (math/gcd val other) 1)))

  )
