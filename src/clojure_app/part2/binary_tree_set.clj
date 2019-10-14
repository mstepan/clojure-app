(ns clojure-app.part2.binary-tree-set)

;-----------------------------------------------------------------------------------------
; Set as binary search tree representation.
;-----------------------------------------------------------------------------------------
; Binary tree node interface
(defn make-node [value left right]
  (list value left right)
  )

(defn entry [node]
  (first node)
  )

(defn left-branch [node]
  (first (rest node))
  )

(defn right-branch [node]
  (last node)
  )

; Set operations
(defn element-of-set? [x s]
  (if (empty? s)
    false
    (let [cur (entry s)
          left (left-branch s)
          right (right-branch s)]
      (cond
        (= cur x) true
        (< x cur) (element-of-set? x left)
        :else (element-of-set? x right)
        )
      )
    )
  )

(defn adjoint-set [x s]
  (if (empty? s)
    (make-node x '() '())
    (let [cur (entry s)
          left (left-branch s)
          right (right-branch s)]
      (cond
        (= cur x) s
        (< x cur) (make-node cur (adjoint-set x left) right)
        :else (make-node cur left (adjoint-set x right))
        )
      )
    )
  )

(defn intersection-set [s1 s2])

(defn union-set [s1 s2])

; Convert binary tree to list using in-order traversal order.
; time: O(N)
; space: O(lg N + N)
(defn tree-to-list1 [tree]
  (if (empty? tree)
    '()
    (let [cur (entry tree)
          left (left-branch tree)
          right (right-branch tree)]
      (append (tree-to-list1 left) (cons cur (tree-to-list1 right)))
      )
    )
  )

; Convert binary tree to list using in-order traversal order.
(defn tree-to-list2 [tree]

  (defn copy-to-list [tree res]
    (if (empty? tree)
      res
      (let [cur (entry tree)
            left (left-branch tree)
            right (right-branch tree)]
        (copy-to-list left (cons cur (copy-to-list right res)))
        )
      )
    )

  (copy-to-list tree '())
  )


(def s1 (adjoint-set 13 (adjoint-set 18 (adjoint-set 2 (adjoint-set 8 (adjoint-set 5 (adjoint-set 10 '())))))))

;(def s2 '(1 8 10 12 14))
;
;(def set-1-2-intersection (intersection-set s1 s2))
;
;(def set-1-2-union (union-set s1 s2))


