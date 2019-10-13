(ns clojure-app.part1.fixed-point)

; Find fixed point for a function.
; f(x) = x
(defn find-fixed-point [f initial-guess]

  (def tolerance 0.0001)

  (defn close-enough? [x y]
    (< (Math/abs (- x y)) tolerance)
    )

  (defn fixed-point-it [guess]
    (let [next-guess (f guess)]
      (if (close-enough? guess next-guess)
        next-guess
        (fixed-point-it next-guess)
        )
      )
    )

  (fixed-point-it initial-guess)

  )
