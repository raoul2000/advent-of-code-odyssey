(ns day-3
  (:require [clojure.string :as s]))


;; https://adventofcode.com/2025/day/3

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sample-input "987654321111111
811111111111119
234234234234278
818181911112111
")

(def puzzle-input (slurp "resources/day_3.txt"))

(defn parse [input]
  (->> (s/split-lines input)
       (map seq)
       (map (fn [bank]
              (map #(- (int %) 48) bank)))))

(comment

  ;; given a seq of digit, get 2 max preserving order

  (->> (parse sample-input)
       (map (fn [bank]
              (map-indexed (fn [k v] [k v]) bank)))
       (map (fn [indexed-bank]
              (sort-by second > indexed-bank)))
       (map (fn [sorted-indexed-bank]
              (take 2 sorted-indexed-bank)
              ))
       (map (fn [max2]
              (sort-by first < max2)
              ))
       
       ;;
       )

  (sort-by first (take 2 (sort-by second (->> (vec '(2 5 4 8))
                                              (map-indexed (fn [k v] [k v]))))))


  (sort > '(3 2 1))
  (parse sample-input)


  ;;
  )