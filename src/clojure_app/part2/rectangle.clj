(ns clojure-app.part2.rectangle
  (:require [clojure-app.part2.point :as point])
  )
;
; Exercise 2.3
;
; Private
(defn make-rect [left-corner right-corner]
  [left-corner
   (point/make-point (point/get-x left-corner) (point/get-y right-corner))
   right-corner
   (point/make-point (point/get-x right-corner) (point/get-y left-corner))
   ]
  )

(defn- height [rect]
  (point/distance (nth rect 0) (nth rect 1))
  )

(defn- width [rect]
  (point/distance (nth rect 0) (nth rect 3))
  )

;
; Public
;
(defn area [rect]
  (* (height rect) (width rect))
  )


(defn perimiter [rect]
  (* 2 (+ (height rect) (width rect)))
  )
