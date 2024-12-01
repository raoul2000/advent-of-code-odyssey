(ns day-1
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2024/day/1

;; location-id
;; left-list, right-list, distance

(comment

  ;; sort nulber list in ascending order
  (sort [5 3 8 1])

  ;; absolute value
  (Math/abs -1)
  ;; map 2 lists to distance
  (map #(Math/abs (- %1 %2)) [2 4 6] [8 9 3])

  ;; reduce 2 list to distance sum
  (apply + '(1 2 3))

  ;; reading input ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; we need to create to lists of integers : one from col1 and one from col 2

  (slurp "resources/day_1.txt")

  (s/split "123  445" #" +")
  (Integer/parseInt "11254")
  (Integer/parseInt "")


  (let [[left-xs right-xs] (->> (slurp "resources/day_1.txt")
                                (s/split-lines)
                                (map #(let [[left right] (s/split % #" +")]
                                        [(Integer/parseInt left) (Integer/parseInt right)]))
                                (reduce (fn [[left-xs right-xs] [left right]]
                                          [(conj left-xs left)
                                           (conj right-xs right)]) [[] []]))]

    (last left-xs))

  ;;
  )

(def sample-data "3   4
4   3
2   5
1   3
3   9
3   3")


(defn read-input [input-s]
  (->> (s/split-lines input-s)
       (map #(let [[left right] (s/split % #" +")]
               [(Integer/parseInt left) (Integer/parseInt right)]))
       (reduce (fn [[left-xs right-xs] [left right]]
                 [(conj left-xs left)
                  (conj right-xs right)]) [[] []])))

(defn solution-1 []
  (let [[left-xs right-xs] (read-input (slurp "resources/day_1.txt"))]
    (->> (map #(Math/abs (- %1 %2)) (sort left-xs) (sort right-xs))
         (apply +))))

(solution-1)
;; => 1666427 ⭐

