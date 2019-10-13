(ns clojure-app.part2.rational
  (:require [clojure-app.part1.math :as math])
  )

;
; Private section
;
(defn- determine-sign [x y]
  (cond
    (and (> x 0) (> y 0)) 1
    (and (< x 0) (< y 0)) 1
    :else -1
    )
  )

(defn make-rat [n d]

  (let [divisor (math/gcd n d)
        num (/ n divisor)
        denom (/ d divisor)]

    (cond
      (= num 0) [0 0]
      (= denom 0) ["Infinity"]
      :else [(* (math/abs num) (determine-sign num denom)) (math/abs denom)]
      )
    )

  )

(defn- numer [rational]
  (first rational)
  )

(defn- denom [rational]
  (last rational)
  )

;
; Public section
;
(defn change-sign [rational]
  (make-rat (- (numer rational)) (denom rational))
  )

(defn to-str [rational]
  (let [n (numer rational)
        d (denom rational)]
    (cond
      (= n d) "1"
      (< n d) (str n "/" d)
      :else (str (quot n d) " * " (mod n d) "/" d)
      )
    )

  )

(defn is-infinity? [rational]
  (= (numer rational) "Infinity")
  )

(defn equals? [x y]
  (and (= (numer x) (numer y)) (= (denom x) (denom y)))
  )

(defn add-rat [x y]
  (cond
    (or (is-infinity? x) (is-infinity? y)) x
    :else (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y)))
    )
  )

(defn sub-rat [x y]
  (cond
    (or (is-infinity? x) (is-infinity? y)) x
    :else (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y)))
    )
  )

(defn mul-rat [x y]
  (cond
    (or (is-infinity? x) (is-infinity? y)) x
    :else (make-rat (* (numer x) (numer y)) (* (denom x) (denom y)))
    )
  )

(defn div-rat [x y]
  (cond
    (or (is-infinity? x) (is-infinity? y)) x
    :else (make-rat (* (numer x) (denom y)) (* (numer y) (denom x)))
    )
  )


