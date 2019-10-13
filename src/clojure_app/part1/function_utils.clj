(ns clojure-app.part1.function-utils)

; Exercise 1.43
; Repeatedly call function 'f' specified number of times 'cnt'.
(defn repeated [f cnt]
  (defn repeated-it [left-calls val]
    (if (= left-calls 0)
      val
      (repeated-it (- left-calls 1) (f val))
      )
    )

  (fn [x]
    (repeated-it cnt x)
    )
  )

; Exercise 1.44
(defn smoothed [f]
  (def dx 0.0001)

  (fn [x]
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)
    )

  )
