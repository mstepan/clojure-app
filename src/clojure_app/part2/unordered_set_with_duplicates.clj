(ns clojure-app.part2.unordered-set-with-duplicates)

;-----------------------------------------------------------------------------------------
; Exercise 2.60. Unordered set with duplicates.
;-----------------------------------------------------------------------------------------
; time: O(N)
(defn element-of-set? [x s]
  (if (empty? s)
    false
    (let [head (first s)
          tail (rest s)]
      (if (= head x)
        true
        (element-of-set? x tail)
        )
      )
    )
  )

; time: O(1)
(defn adjoint-set [x s]
  (cons x s)
  )

; time: O(N^2)
(defn intersection-set [s1 s2]
  (if (empty? s1)
    nil
    (let [head (first s1)
          tail (rest s1)]
      (if (element-of-set? head s2)
        (cons head (intersection-set tail s2))
        (intersection-set tail s2)
        )
      )
    )
  )

; time: O(1)
(defn union-set [s1 s2]
  (append s1 s2)
  )

(defn make-set-from-list [items]
  (if (empty? items)
    nil
    (adjoint-set (first items) (make-set-from-list (rest items)))
    )
  )


(def set1 (make-set-from-list '(2 3 2 1 3 2 2)))

(def set2 (make-set-from-list '(5 8 5 12 3 2 3 2)))

(def set-1-2-intersection (intersection-set set1 set2))

(def set-1-2-union (union-set set1 set2))
