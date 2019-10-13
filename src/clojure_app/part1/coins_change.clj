(ns clojure-app.part1.coins-change)

; Calculate the ways to change amount of money.
; Use memoization.
(defn ways-to-change [amount use-memo]

  (def coins [50, 25, 10, 5, 1])

  (defn change-it [amount index]
    (if (= amount 0)
      1
      (if (or (>= index (count coins)) (< amount 0))
        0
        (+ (change-it (- amount (get coins index)) index)
           (change-it amount (+ index 1)))
        )
      )
    )

  (if use-memo
    (do
      (def change-it (memoize change-it))
      (change-it amount  0)
      )
    (change-it amount  0)
    )
  )
