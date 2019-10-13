(ns clojure-app.part2.point)

;
; Private
;
; Point
(defn make-point [x y]
  [x y]
  )

(defn get-x [point]
  (first point)
  )

(defn get-y [point]
  (last point)
  )

;
; Public
;
(defn point-to-str [point]
  (str "[" (get-x point) ";" (get-y point) "]")
  )

; Euclid distance between 2 points
(defn distance [p1 p2]
  (let [dx (- (get-x p1) (get-x p2))
        dy (- (get-y p1) (get-y p2))
        ]
    (Math/sqrt (+ (* dx dx) (* dy dy)))
    )
  )
