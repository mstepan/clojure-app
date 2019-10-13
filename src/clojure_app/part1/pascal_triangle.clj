(ns clojure-app.part1.pascal-triangle)

; Exercise 1.12:

; Calculate single Pascal triangle row
(defn calculate-row [row]

  (defn calculate-cell [row col]
    (if (or (< row 2) (= col 0) (= row col))
      1
      (+ (calculate-cell (- row 1) (- col 1))
         (calculate-cell (- row 1) col))
      )
    )

  (defn calculate-row-it [cur-col last-col]
    (if (= cur-col last-col)
      (calculate-cell row cur-col)
      (str (calculate-cell row cur-col) " "
           (calculate-row-it (+ cur-col 1) last-col))

      )
    )

  (calculate-row-it 0 row)

  )

; Print the whole Pascal triangle row-by-row up to 'n' row.
(defn print-pascal-triangle [n]

  (defn print-pascal-triangle-it [cur-row]
    (if (= cur-row n)
      (println (calculate-row cur-row))
      (do
        (println (calculate-row cur-row))
        (print-pascal-triangle-it (+ cur-row 1))
        )
      )
    )

  (print-pascal-triangle-it 0)

  )
