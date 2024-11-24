(ns day-1
  (:require [clojure.string :refer [split-lines]]))

;; -- day 1 ---------------------------------------

(defn read-puzzle-1 []
  (->> (slurp "./resources/puzzle_1.txt")
       split-lines
       (map #(Integer/parseInt %1))))

(defn count-increment-1 [[counter prec-val] cur-val]
  [(if (> cur-val prec-val)
     (inc counter)
     counter)
   cur-val])

(->> (read-puzzle-1)
     (reduce count-increment-1 [-1 0])
     first)
;; => 1564

(defn counter-inc [values]
  (if (> (count values) 3)
    (let [n   (apply + (first (partition 3 values)))
          n+1 (apply + (first (partition 3 (rest values))))]
      (if (> n+1 n) 1 0))
    0))

(defn count-increment-2
  ([values]
   (count-increment-2 values 0))
  ([values cnt]
   (if (empty? values)
     cnt
     (recur (rest values) (+ cnt (counter-inc values))))))

(->> (read-puzzle-1)
     (count-increment-2))
;; => 1611
