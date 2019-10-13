(ns clojure-app.part2.line
  (:require [clojure-app.part2.point :as point]))

;
; Exercise 2.2
;
; Line
(defn make-line [start-point end-point]
  (fn [method]
    (cond
      (= method "start") start-point
      (= method "end") end-point
      :else nil)
    )
  )

(defn get-start [line]
  (line "start")
  )

(defn get-end [line]
  (line "end")
  )

(defn- avg [x y]
  (/ (+ x y) 2)
  )

;
; Public
;
(defn line-to-str [line]
  (str (point/point-to-str (get-start line)) " ---> " (point/point-to-str (get-end line)))
  )

(defn midpoint [line]
  (let [start (get-start line)
        end (get-end line)]
    (point/make-point (avg (point/get-x start) (point/get-x end)) (avg (point/get-y start) (point/get-y end)))
    )
  )
