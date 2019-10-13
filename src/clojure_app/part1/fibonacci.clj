(ns clojure-app.part1.fibonacci)

; Exercise 1.11
; F(0)=0, F(1) = 1, F(2) = 2
; F(n) = F(n-1) + F(n-2) + F(n-3)

; Use tree recursion.
(defn fib-three [n]
  (if (< n 3)
    n
    (+ (fib-three (- n 1)) (fib-three (- n 2)) (fib-three (- n 3)))
    )
  )

; Use memoization.
(defn fib-three-optimized [n]

  (defn fib-tree-it [a b c index]
    (if (= index n)
      c
      (fib-tree-it b c (+ a b c) (+ index 1))
      )
    )

  (if (< n 3)
    n
    (fib-tree-it 0 1 2 2)
    )

  )
