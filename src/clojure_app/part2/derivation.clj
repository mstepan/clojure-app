(ns clojure-app.part2.derivation)

;-----------------------------------------------------------------------------------------
; Symbolic derivation
;-----------------------------------------------------------------------------------------

(defn variable? [exp]
  (symbol? exp)
  )

(defn same-variable? [x y]
  (and (variable? x) (variable? y) (= x y))
  )


(defn sum? [exp]
  (= (first exp) '+)
  )

(defn zero-num? [val]
  (and (number? val) (= val 0))
  )

(defn one-num? [val]
  (and (number? val) (= val 1))
  )

(defn make-sum [op1 op2]
  (cond
    (zero-num? op1) op2
    (zero-num? op2) op1
    (and (number? op1) (number? op2)) (+ op1 op2)
    :else (list '+ op1 op2)
    )
  )

(defn first-operand [exp]
  (first (rest exp))
  )

(defn second-operand [exp]
  (first (rest (rest exp)))
  )

(defn product? [exp]
  (= (first exp) '*)
  )

(defn make-product [op1 op2]
  (cond
    (or (zero-num? op1) (zero-num? op2)) 0
    (one-num? op1) op2
    (one-num? op2) op1
    (and (number? op1) (number? op2)) (* op1 op2)
    :else (list '* op1 op2)
    )
  )

(defn derive [exp var]
  (cond
    (number? exp) 0
    (variable? exp)
    (if (same-variable? exp var) 1 0)
    (sum? exp) (make-sum (derive (first-operand exp) var)
                         (derive (second-operand exp) var))
    (product? exp) (let [op1 (first-operand exp)
                         op2 (second-operand exp)]
                     (make-sum
                       (make-product op1 (derive op2 var))
                       (make-product op2 (derive op1 var)))
                     )
    :else nil
    )

  )
