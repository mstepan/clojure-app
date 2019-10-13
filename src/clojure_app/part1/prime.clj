(ns clojure-app.part1.prime
  (:require [clojure-app.part1.math :as math]))

(defn divisible? [a b]
  (= (mod a b) 0)
  )

; Check if value is prime or not.
; time: O(lgN)
; space: O(1)
(defn check-prime? [val]

  (defn check-prime-it? [d last-value]
    (if (> d last-value)
      true
      (if (divisible? val d)
        false
        (check-prime-it? (+ d 1) last-value)
        )
      )
    )

  (defn sqrt-rounded-plus-one []
    (+ (int (Math/sqrt val)) 1)
    )

  (cond
    (<= val 1) false
    (or (= val 2) (= val 3)) true
    :else (check-prime-it? 2 (sqrt-rounded-plus-one))
    )
  )

;(def carmichael-numbers [561 1105 1729 2465 2821 6601 8911 10585])

; Check if value is prime or not with some probability using
; Fermat's little theorem.
; (Carmichael numbers will be evaluated as true - so false positive here)
(defn check-probable-prime? [val]

  ; 'a' - random value in range [1; n-1]
  ; 'n' - value to check for prime
  (defn is-prime? [a n]
    (= (math/mod-exp a n n) a)
    )

  (defn check-probable-prime-it? [it]
    (if (= it 0)
      true
      (if (is-prime? (+ 1 (rand-int (- val 1))) val)
        (check-probable-prime-it? (- it 1))
        false
        )
      )
    )

  ;(check-probable-prime-it? 10)

  ;(if (contains? carmichael-numbers val)
  ;  false
  ;  (check-probable-prime-it? 10)
  ;  )

  (check-probable-prime-it? 10)

  )

; Count primes that below or equals to 'n'
; time: O(N)
; space O(1)
(defn count-primes [n check-prime-fn?]
  (defn count-primes-it [cur cnt]
    (if (> cur n)
      cnt
      (if (check-prime-fn? cur)
        (count-primes-it (+ cur 1) (+ cnt 1))
        (count-primes-it (+ cur 1) cnt)
        )
      )
    )

  (count-primes-it 2 0)
  )


(defn count-primes-exact [n]
  (count-primes n check-prime?)
  )

(defn count-primes-probable [n]
  (count-primes n check-probable-prime?)
  )


(defn get-all-primes [n]

  (defn get-primes-it [cur]
    (if (> cur n)
      (str "")
      (if (check-prime? cur)
        (str cur ", " (get-primes-it (+ cur 1)))
        (str (get-primes-it (+ cur 1)))
        )
      )
    )

  (get-all-primes 2)
  )
