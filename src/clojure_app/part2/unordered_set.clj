(ns clojure-app.part2.unordered-set)

;-----------------------------------------------------------------------------------------
; Set unordered list
;-----------------------------------------------------------------------------------------
; time: O(N)
; space: O(1)
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
; time: O(N)
; space: O(1)
(defn adjoint-set [x s]
  (if (element-of-set? x s)
    nil
    (cons x s)
    )
  )

; time: O(N^2)
; space: O(N)
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

; Exercise 2.59
; time: O(N^2)
; space: O(N)
(defn union-set [s1 s2]
  (if (empty? s1)
    s2
    (let [head (first s1)
          tail (rest s1)]
      (if (not (element-of-set? head s2))
        (cons head (union-set tail s2))
        (union-set tail s2)
        )
      )
    )

  )



(def set1 (adjoint-set 1 (adjoint-set 2 (adjoint-set 12 (adjoint-set 3 nil)))))

(def set2 (adjoint-set 8 (adjoint-set 2 (adjoint-set 3 (adjoint-set 12 nil)))))

(def set-1-2-intersection (intersection-set set1 set2))

(def set-1-2-union (union-set set1 set2))


