(ns clojure-app.part1.math)

(defn sin [x]
  (Math/sin x)
  )

(defn cos [x]
  (Math/cos x)
  )


(defn square [x]
  (* x x)
  )

; Calculate average of two values.
(defn average [x y]
  (/ (+ x y) 2)
  )

; time: O(lg N)
; space O(1)
(defn fast-exp [a n]

  (defn fast-exp-it [x y res]

    (if (= y 1)
      (* x res)
      (if (even? y)
        (fast-exp-it (* x x) (/ y 2) res)
        (fast-exp-it x (- y 1) (* x res))
        )
      )
    )

  (fast-exp-it a n 1)

  )

; Modular exponentiation
; Calculates (a ^ n) % m
; time: O(lg N)
; space: O(1)
(defn mod-exp [a n m]

  (defn fast-mod-exp-it [x y res]

    (if (= y 1)
      (mod (* x res) m)
      (if (even? y)
        (fast-mod-exp-it (mod (* x x) m) (/ y 2) res)
        (fast-mod-exp-it x (- y 1) (mod (* x res) m))
        )
      )
    )

  (fast-mod-exp-it a n 1)

  )

(defn abs [x]
  (if (< x 0) (- x) x)
  )


(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))
    )
  )

; 0.01 %
(def percentage-threshold 0.01)

(defn percentage [val other]
  (/ (* other 100.0) val)
  )

(defn avg [x y]
  (/ (+ x y) 2)
  )

(defn good-enough-in-percentage? [_ guess next-guess]
  (if (< (abs (- 100.0 (percentage guess next-guess)))
         percentage-threshold)
    true
    false)
  )

; Use Newtons method to compute square root iteratively
(defn sqrt [x]

  ; this value should be double, otherwise you will have rational numbers
  (def initial-guess 1.0)

  (defn good-enough-square? [val guess _]
    (if (< (abs (- val (* guess guess))) 0.001)
      true
      false)
    )

  (defn improve [guess]
    (avg (/ x guess) guess)
    )

  (defn sqrt-iter [guess good-enough-predicate? improve-fn]
    (if (good-enough-predicate? x guess (improve-fn guess))
      guess
      (sqrt-iter (improve-fn guess) good-enough-predicate? improve-fn))
    )

  (sqrt-iter initial-guess good-enough-square? improve)
  )

;(defn sqrt2 [x]
;  (sqrt-iter x initial-guess good-enough-in-percentage? improve)
;  )
;
;(defn cude-improve [x, y]
;  (/ (+ (/ x (* y y)) (* 2 y)) 3)
;  )
;
;(defn cubert [x]
;  (sqrt-iter x initial-guess good-enough-in-percentage? cude-improve)
;  )


