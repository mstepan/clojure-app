(ns clojure-app.part1.collatz)

(defn- next-collatz [val]
  (if (even? val)
    (/ val 2)
    (+ (* 3 val) 1)
    )
  )

(def max-it-count 1000)

(defn- converged? [val itCount boundary]
  (if (or (<= val boundary) (= val 1))
    true
    (if (= itCount 0)
      (do
        (println (str "Collatz conjecture wasn't proved for value "
                      (+ boundary 1) " within " max-it-count " iterations"))
        false)
      (converged? (next-collatz val) (- itCount 1) boundary)
      )
    )
  )


(defn- check-collatz-it? [cur lastValue]
  (if (> cur lastValue)
    true
    (and (converged? cur max-it-count (- cur 1)) (check-collatz-it? (+ cur 1) lastValue))
    )
  )


(defn check-collatz? [n]
  (if (< n 3)
    true
    (check-collatz-it? 2 n)
    )
  )

(defn- print-collatz-seq-it [val it]
  (if (= val 1)
    (do
      (println "1")
      (println (str "iterations count: " it))
      )
    (do
      (println val)
      (print-collatz-seq-it (next-collatz val) (+ it 1))
      )
    )
  )


(defn  print-collatz-seq [val]
  (print-collatz-seq-it val 1)
  )

