(ns clojure-app.part2.sorted-list-set)

;-----------------------------------------------------------------------------------------
; Set as sorted list.
;-----------------------------------------------------------------------------------------

; time: O(N/2)
(defn element-of-set? [x s]
  (if (empty? s)
    false
    (let [head (first s)
          tail (rest s)]
      (cond
        (= head x) true
        (> head x) false
        :else (element-of-set? x tail)
        )
      )
    )
  )

; Exercise 2.61
; time: O(N/2)
(defn adjoint-set [x s]
  (if (empty? s)
    (list x)
    (let [head (first s)
          tail (rest s)]
      (if (< head x)
        (cons head (adjoint-set x tail))
        (if (= head x)
          s
          (cons x s)
          )
        )
      )
    )
  )

; time: O(N)
(defn intersection-set [s1 s2]
  (if (or (empty? s1) (empty? s2))
    nil
    (let [h1 (first s1)
          t1 (rest s1)
          h2 (first s2)
          t2 (rest s2)]
      (cond
        (= h1 h2) (cons h1 (intersection-set t1 t2))
        (< h1 h2) (intersection-set t1 s2)
        :else (intersection-set s1 t2)
        )
      )
    )

  )

; Exercise 2.62
; time: O(N)
(defn union-set [s1 s2]
  (cond
    (empty? s1) s2
    (empty? s2) s1
    :else
    (let [h1 (first s1)
          t1 (rest s1)
          h2 (first s2)
          t2 (rest s2)]
      (cond
        (= h1 h2) (cons h1 (union-set t1 t2))
        (< h1 h2) (cons h1 (union-set t1 s2))
        :else (cons h2 (union-set s1 t2))
        )
      )
    )
  )


(def s1 '(1 3 6 10))

(def s2 '(1 8 10 12 14))

(def set-1-2-intersection (intersection-set s1 s2))

(def set-1-2-union (union-set s1 s2))

