(ns clojure-app.core
  (:gen-class)
  (:require [clojure-app.part1.math :as math])
  (:require [clojure-app.part1.collatz :as collatz])
  (:require [clojure-app.part1.coins-change :as coins-change])
  (:require [clojure-app.part1.pascal-triangle :as pascal])
  (:require [clojure-app.part1.prime :as prime])
  (:require [clojure-app.part1.sums :as sums])
  (:require [clojure-app.part1.find-roots :as find-roots])
  (:require [clojure-app.part1.fixed-point :as fixed-point])
  (:require [clojure-app.part1.function-utils :as function-utils])
  (:require [clojure-app.part2.rational :as rational])
  (:require [clojure-app.part2.line :as line])
  (:require [clojure-app.part2.point :as point])
  (:require [clojure-app.part2.rectangle :as rectangle])
  )

(defn part1-calls []
  (println (str "square(square(square (2))): " ((function-utils/repeated math/square 3) 2)))

  (println (str "fixed point of cos(x) = " (fixed-point/find-fixed-point math/cos 1.0)))

  (println (str "sin(x) = 0: " (find-roots/search-for-root math/sin 4.0 2.0)))
  (println (str "x^3 - 2x - 3 = 0: " (find-roots/search-for-root (fn [x] (- (* x x x) (* 2 x) 3)) 1.0 2.0)))

  (println (str "sum [1...10]: " (sums/sum-integers 1 10)))
  (println (str "sum of cubes [1...3]: " (sums/sum-cubes 1 3)))
  (println (str "PI: " (sums/pi-approximation)))
  (println (str "integral: " (sums/integral sums/cube 0 1 0.001)))

  ;(println (str "Primes count below 100 (exact): " (prime/count-primes-exact 100)))
  ;(println (str "Primes count below 100 (probable): " (prime/count-primes-probable 100)))
  ;
  ;(println (str "All primes below 100: "))
  ;(prime/get-all-primes 100)
  ;(println)
  ;
  ;(println (str "Pascal triangle for " 10 ": "))
  ;(pascal/print-pascal-triangle 10)
  ;(println)
  ;
  ;(println (str "Ways to change 20$: " (coins-change/ways-to-change 2000 true)))
  ;(println)
  ;
  ;(println (str "collatz(" 1000 "): " (collatz/check-collatz? 1000)))
  ;(println)
  ;
  ;(println (str "sqrt1 (2) = " (math/sqrt 2)))
  ;(println (str "sqrt1 (3) = " (math/sqrt 3)))
  ;(println (str "sqrt1 (0.000009) = " (math/sqrt 0.000009)))
  ;(println (str "sqrt1 (* 777777 777777) = " (math/sqrt (* 777777 777777))))
  )


; time: O(N)
; space O(1)
(defn list-ref [items n]
  (if (= n 0)
    (first items)
    (list-ref (rest items) (- n 1))
    )
  )

; time: O(N)
; space: O(1)
(defn length [items]

  (defn length-it [col count]
    (if (= col '())
      count
      (length-it (rest col) (+ count 1))
      )
    )

  (length-it items 0)
  )

; time: O(N), where N - 'list1' length
; space: O(N)
(defn append [list1 list2]
  (if (= list1 '())
    list2
    (cons (first list1) (append (rest list1) list2))
    )
  )


;-----------------------------------------------------------------------------------------
; Exercise 2.17
;-----------------------------------------------------------------------------------------
; time: O(N)
(defn last-pair [items]
  (let [tail (rest items)]
    (if (= tail '())
      (first items)
      (last-pair tail)
      )
    )
  )

;-----------------------------------------------------------------------------------------
; Exercise 2.18
;-----------------------------------------------------------------------------------------
; time: O(N)
; space: O(N)
(defn reverse [items]
  (let [head (first items)
        tail (rest items)]
    (if (nil? head)
      '()
      (concat (reverse tail) (list head))
      )
    )
  )

(def squares '(1 4 9 16 25))

(def primes '(2 3 5 7 11 13 17))

(def us-coins '(50 25 10 5 1))

(def uk-coins '(0.5 1 2 5 10 20 50 100))

; Exercise 2.19
(defn ways-to-change [amount coins]

  (cond
    (= amount 0) 1
    (or (< amount 0) (= coins '())) 0
    :else (let [head (first coins)
                tail (rest coins)]
            (+
              (ways-to-change (- amount head) coins)
              (ways-to-change amount tail))
            )
    )
  )


(defn filter [items predicate?]
  (let [head (first items)
        tail (rest items)]
    (cond
      (nil? head) '()
      (predicate? head) (cons head (filter tail predicate?))
      :else (filter tail predicate?)
      )
    )

  )

;-----------------------------------------------------------------------------------------
; Exercise 2.20
;-----------------------------------------------------------------------------------------
(defn same-parity [x & other]
  (let [parity-predicate (if (even? x) even? odd?)]
    (cons x (filter other parity-predicate))
    )
  )

; time: O(N)
; space: O(N), linear recursive
(defn map-list [f items]
  (let [head (first items)
        tail (rest items)]
    (if (nil? head)
      nil
      (cons (f head) (map-list f tail))
      )
    )
  )


(defn scale-list [items factor]
  (map-list (fn [val] (* val factor)) items)
  )


;-----------------------------------------------------------------------------------------
; Exercise 2.21 - 1st solution
;-----------------------------------------------------------------------------------------
(defn square-list1 [items]
  (let [head (first items)
        tail (rest items)]
    (if (nil? head)
      nil
      (cons (* head head) (square-list1 tail))
      )
    )
  )

;-----------------------------------------------------------------------------------------
; Exercise 2.21 - 2nd solution
;-----------------------------------------------------------------------------------------
(defn square-list2 [items]
  (map-list (fn [x] (* x x)) items)
  )


;-----------------------------------------------------------------------------------------
; Exercise 2.22
;-----------------------------------------------------------------------------------------
(defn square-list-broken1 [items]

  (defn iter [data answer]
    (let [head (first data)
          tail (rest data)]
      (if (nil? head)
        answer
        (iter tail (cons (* head head) answer))
        )
      )
    )

  (iter items nil)

  )

(defn square-list-broken2 [items]

  (defn iter [data answer]
    (let [head (first data)
          tail (rest data)]
      (if (nil? head)
        answer
        (iter tail (cons answer (* head head)))
        )
      )
    )

  (iter items nil)

  )

;-----------------------------------------------------------------------------------------
; Exercise 2.23
;-----------------------------------------------------------------------------------------
(defn for-each [items f]
  (let [head (first items)
        tail (rest items)]

    (if (nil? head)
      nil
      (do
        (f head)
        (for-each tail f)
        )
      )
    )

  )

;-----------------------------------------------------------------------------------------
; Exercise 2.27 - not working
;-----------------------------------------------------------------------------------------
;(defn deep-reverse [items]
;
;  (let [head (first items)
;        tail (rest items)]
;    (if (nil? head)
;      '()
;      (if (list? head)
;        (list (deep-reverse tail) (deep-reverse head))
;        (concat (deep-reverse tail) (list head))
;        )
;
;      )
;    )
;  )

;-----------------------------------------------------------------------------------------
; Exercise 2.28
;-----------------------------------------------------------------------------------------
(defn fringe [items]
  (let [head (first items)
        tail (rest items)]

    (cond
      (nil? head) nil
      (list? head) (concat (fringe head) (fringe tail))
      :else (cons head (fringe tail))
      )

    )
  )

;-----------------------------------------------------------------------------------------
; Exercise 2.31
;-----------------------------------------------------------------------------------------
(defn tree-map [f tree]
  (let [head (first tree)
        tail (rest tree)]

    (cond
      (nil? head) nil
      (list? head) (cons (tree-map f head) (tree-map f tail))
      :else (cons (f head) (tree-map f tail))
      )

    )
  )



(defn scale-tree1 [tree factor]
  (cond
    (or (nil? tree) (= tree '())) nil
    (list? tree) (cons (scale-tree1 (first tree) factor) (scale-tree1 (rest tree) factor))
    :else (* tree factor)
    )
  )

(defn scale-tree2 [tree factor]
  (map
    (fn [subtree]
      (if (list? subtree)
        (scale-tree2 subtree factor)
        (* subtree factor)
        )
      )
    tree)
  )


(defn count-leaves [tree]

  (let [head (first tree)
        tail (rest tree)]

    (cond
      (nil? head) 0
      (list? head) (+ (count-leaves head) (count-leaves tail))
      :else (+ 1 (count-leaves tail))
      )
    )
  )

;-----------------------------------------------------------------------------------------
; Exercise 2.32
;-----------------------------------------------------------------------------------------
(defn subsets [s]
  (if (empty? s)
    (list '())
    (let [tail (subsets (rest s))]
      (append tail (map (fn [x] (cons (first s) x)) tail))
      )
    )
  )


(defn sum-odd-squares1 [tree]
  (let [head (first tree)
        tail (rest tree)]
    (cond
      (nil? head) 0
      (list? head) (+ (sum-odd-squares1 head) (sum-odd-squares1 tail))
      (odd? head) (+ (math/square head) (sum-odd-squares1 tail))
      :else (sum-odd-squares1 tail)
      )
    )
  )

(defn even-fibs1 [last-index]

  (defn next-fib [a b cur-index]
    (if (> cur-index last-index)
      nil
      (if (even? a)
        (cons a (next-fib b (+ a b) (+ cur-index 1)))
        (next-fib b (+ a b) (+ cur-index 1))
        )
      )
    )

  (next-fib 1 1 0)

  )

; MAP function
(defn map-ext [f items]
  (let [head (first items)
        tail (rest items)]
    (if (nil? head)
      nil
      (cons (f head) (map-ext f tail))
      )
    )
  )

; FILTER function
(defn filter-ext [p items]
  (let [head (first items)
        tail (rest items)]
    (cond
      (nil? head) nil
      (p head) (cons head (filter-ext p tail))
      :else (filter-ext p tail)
      )
    )
  )

; ACCUMULATOR/AGGREGATOR function
(defn accum-ext [op initial items]
  (let [head (first items)
        tail (rest items)]
    (cond
      (nil? head) initial
      :else (op head (accum-ext op initial tail))
      )
    )
  )


(defn enumerate-leaves [tree]
  (let [head (first tree)
        tail (rest tree)]

    (cond
      (nil? head) nil
      (list? head) (append (enumerate-leaves head) (enumerate-leaves tail))
      :else (cons head (enumerate-leaves tail))
      )

    )
  )

(defn sum-odd-squares2 [tree]
  (accum-ext + 0 (map-ext math/square (filter-ext odd? (enumerate-leaves tree))))
  )


(defn enumerate-fib [last]

  (defn next-it [a b cur]
    (if (> cur last)
      nil
      (cons a (next-it b (+ a b) (+ cur 1)))
      )
    )

  (next-it 1 1 0)

  )

(defn even-fibs2 [cur-index]
  (accum-ext cons nil (filter-ext even? (enumerate-fib cur-index)))
  )


;-----------------------------------------------------------------------------------------
; Exercise 2.33 - start
;-----------------------------------------------------------------------------------------
(defn map-acc [f s]
  (accum-ext (fn [x y] (cons (f x) y)) nil s)
  )

(defn append-acc [s1 s2]
  (accum-ext cons s2 s1)
  )

(defn length-acc [s]
  (accum-ext (fn [x y] (+ 1 y)) 0 s)
  )

;-----------------------------------------------------------------------------------------
; Exercise 2.34
;-----------------------------------------------------------------------------------------
(defn horner-eval [coeff x]
  (accum-ext (fn [lower higher] (+ lower (* higher x))) 0 coeff)
  )

;-----------------------------------------------------------------------------------------
; Exercise 2.35
;-----------------------------------------------------------------------------------------
(defn count-leaves-acc [t]

  (defn node-counter [cur next-node]
    (if (list? cur)
      (+ (count-leaves-acc cur) next-node)
      (+ 1 next-node)
      )
    )
  (accum-ext node-counter 0 t)
  )

;-----------------------------------------------------------------------------------------
; Exercise 2.36 - start
;-----------------------------------------------------------------------------------------
(defn extract [seqs f]
  (let [head (first seqs)
        tail (rest seqs)]
    (if (nil? head)
      nil
      (cons (f head) (extract tail f))
      )
    )
  )

(defn heads [seqs]
  (extract seqs first)
  )

(defn tails [seqs]
  (extract seqs rest)
  )

(defn accumulate-n [op initial seqs]
  (if (empty? (first seqs))
    nil
    (let [first-part (accum-ext op initial (heads seqs))
          second-part (accumulate-n op initial (tails seqs))]
      (cons first-part second-part)
      )
    )
  )
;-----------------------------------------------------------------------------------------
; Exercise 2.36 - end
;-----------------------------------------------------------------------------------------

;-----------------------------------------------------------------------------------------
; Exercise 2.38
; 'fold-left' will return the same result as 'fold-right' only if 'op' is associative:
; for example ((a + b) + c) = (a + (b + c)), but not ((a - b) - c) ≠ (a - (b - c))
;-----------------------------------------------------------------------------------------

(def fold-right accum-ext)

(defn fold-left [op initial seq]
  (if (empty? seq)
    initial
    (let [head (first seq)
          tail (rest seq)]
      (op (fold-left op initial tail) head)
      )
    )
  )

;-----------------------------------------------------------------------------------------
; Exercise 2.39 - start
;-----------------------------------------------------------------------------------------
(defn reverse-fold-right [seq]
  (fold-right
    (fn [head tail]
      (append tail (list head))
      )
    '() seq)
  )

(defn reverse-fold-left [seq]
  (fold-left
    (fn [head tail]
      (append head (list tail))
      )
    '() seq)
  )

;-----------------------------------------------------------------------------------------
; Exercise 2.39 - end
;-----------------------------------------------------------------------------------------

(defn enumerate-interval [start end]
  (defn enum-it [cur]
    (if (> cur end)
      nil
      (cons cur (enum-it (+ cur 1)))
      )
    )

  (enum-it start)

  )

(defn flat-map-ext [proc seq]
  (accum-ext append nil (map-ext proc seq))
  )

(defn enum-ordered-pairs [n]
  (flat-map-ext
    (fn [i] (map-ext (fn [j] (list j i)) (enumerate-interval 1 (- i 1))))
    (enumerate-interval 2 n))
  )

(defn make-pair-sum [pairs]

  (defn map-pair [pair]
    (let [x (first pair)
          y (last pair)]
      (list x y (+ x y))
      )
    )

  (map-ext map-pair pairs)
  )

(defn prime-pair-sum? [pair]
  (let [pair-sum (last pair)]
    (prime/check-prime? pair-sum)
    )
  )

(defn display-pair-res [pair]
  (let [x (first pair)
        y (first (rest pair))
        pair-sum (last pair)]
    (str x " + " y " = " pair-sum " (isPrime)")
    )
  )

;-----------------------------------------------------------------------------------------
; Permutations - start
;-----------------------------------------------------------------------------------------
(defn remove-value [v items]
  (filter-ext (fn [x] (not (= x v))) items)
  )

; Generate all permutations of a sequence.
; time: O(N!)
; space: O(N!)
(defn permutations [s]
  (if (= (length s) 1)
    (list s)
    (flat-map-ext
      (fn [last-elem]
        (map-ext (fn [tail] (append (list last-elem) tail))
                 (permutations (remove-value last-elem s))))
      s)
    )
  )
;-----------------------------------------------------------------------------------------
; Permutations - end
;-----------------------------------------------------------------------------------------

;-----------------------------------------------------------------------------------------
; Exercise 2.40
; Generates unique pairs (i j), such that 1 <= j < i <= n
;-----------------------------------------------------------------------------------------
(defn unique-pairs [n]
  (flat-map-ext
    (fn [bigger]
      (map-ext (fn [smaller] (list bigger smaller)) (enumerate-interval 1 (- bigger 1)))
      )
    (enumerate-interval 2 n))
  )

;-----------------------------------------------------------------------------------------
; Exercise 2.41 - start
;-----------------------------------------------------------------------------------------
; Generate pairs (i, j) such that: from <= i < j <= to
(defn ordered-pairs [from to]
  (flat-map-ext
    (fn [smaller] (map-ext (fn [bigger] (list smaller bigger))
                           (enumerate-interval (+ smaller 1) to)))
    (enumerate-interval from (- to 1)))

  )

; Generate triples (i, j, k) such that:  1 <= i < j < k <= n
(defn triples [n]
  (flat-map-ext
    (fn [i] (map-ext (fn [single-pair] (cons i single-pair)) (ordered-pairs (+ i 1) n)))
    (enumerate-interval 1 (- n 2)))
  )


(defn map-triples-to-sum [items]
  (map-ext
    (fn [triple] (let [x (first triple)
                       y (first (rest triple))
                       z (first (rest (rest triple)))
                       sum-val (+ x y z)]
                   (list x y z sum-val)
                   )
      )
    items)
  )

(defn sum-eq? [expected-sum]
  (fn [triple]
    (let [sum-val (first (rest (rest (rest triple))))]
      (= sum-val expected-sum)
      )
    )
  )

(defn sum-of-3 [s]
  (let [sum-predicate? (sum-eq? s)]
    (filter-ext sum-predicate? (map-triples-to-sum (triples (- s 2))))
    )
  )



;-----------------------------------------------------------------------------------------
; Exercise 2.41 - end
;-----------------------------------------------------------------------------------------

(defn part2-calls []
  (do
    (let [x (rational/make-rat 6 -10)
          y (rational/make-rat 4 8)
          z (rational/mul-rat x y)
          a (rational/add-rat x y)
          b (rational/sub-rat x (rational/change-sign y))

          first (rational/add-rat x y)
          second (rational/add-rat x y)

          p1 (point/make-point 0 0)
          p2 (point/make-point 3 4)
          line1 (line/make-line p1 p2)
          rect1 (rectangle/make-rect p1 p2)
          ]

      (println (str "x = " (rational/to-str x)))
      (println (str "y = " (rational/to-str y)))
      (println (str "z = x * y = " (rational/to-str z)))
      (println (str "a = x + y = " (rational/to-str a)))
      (println (str "b = x - (-y) = " (rational/to-str b)))

      (println (str (rational/to-str first) " == " (rational/to-str second) ": " (rational/equals? first second)))
      (println (str (rational/to-str x) " == " (rational/to-str y) ": " (rational/equals? x y)))

      (println (line/line-to-str line1))
      (println (point/point-to-str (line/midpoint line1)))
      (println (str "Rectangle area: " (rectangle/area rect1)))
      (println (str "Rectangle perimiter: " (rectangle/perimiter rect1)))
      )
    )

  )

(defn -main
  []
  (do
    ;(part1-calls)
    ;(part2-calls)

    (println "pair sum of primes: ")
    (for-each (map-ext display-pair-res (filter-ext prime-pair-sum? (make-pair-sum (enum-ordered-pairs 6)))) println)

    (def my-tree (list 1 2 (list 3 4 (list 5 6)) 7 8))

    (println (str "sum of squares of add leaves1: " (sum-odd-squares1 my-tree)))
    (println (str "sum of squares of add leaves2: " (sum-odd-squares2 my-tree)))

    (println (str "first even fibs1: " (even-fibs1 11)))
    (println (str "first even fibs2: " (even-fibs2 11)))
    )
  )
