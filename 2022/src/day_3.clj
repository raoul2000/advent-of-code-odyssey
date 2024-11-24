(ns day-3
  (:require [clojure.string :refer [split-lines split]]
            [clojure.data :refer [diff]]
            [clojure.set :refer [intersection]]))

;; https://adventofcode.com/2022/day/3

(def sample-data "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn item->priority [c]
  (let [n (int c)]
    (cond
      (< 96 n 123) (- n 96)
      (< 64 n 91)  (- n 38))))

(comment
  (item->priority \a)
  (item->priority \z)
  (item->priority \A)
  (item->priority \Z)
  ;;
  )

(defn split-rucksack [coll]
  (map #(split-at (quot (count %) 2) %) coll))

(defn common-item [[part-1 part-2]]
  (first (intersection  (set part-1) (set part-2))))

(comment
  (= (set [\a \b \c]) (set [\b \c \a]))
  (= (set [\a \b \c]) (set [\b \c \Z]))
  (intersection  (set [\a \b \c]) (set [\d \e \a]))
  (first #{1 2})
  (first #{})
  ;;
  )

(defn solution-1 []
  (->> (slurp "./resources/puzzle_3.txt")
       (split-lines)
       (split-rucksack)
       (map common-item)
       (map item->priority)
       (apply +)))

(comment
  (solution-1)
  ;; => 8053
  ;;
  )

;; improve with transcuders (attempt)
;; see https://dev.solita.fi/2021/10/14/grokking-clojure-transducers.html

(defn solution-1-perf []
  (apply + (into
            []
            (comp
             (map #(split-at (quot (count %) 2) %))
             (map common-item)
             (map item->priority))
            (split-lines (slurp "./resources/puzzle_3.txt")))))

(comment
  (time (solution-1))
  (time (solution-1-perf))
  ;;
  )


;; part 2 -----------------------------------------------------

;; the badge is the only item type carried by all three Elves (a group)
;; Every set of three lines in your list corresponds to a single group
;; problem = finding the one item type that is common between all three Elves in each group


(def sample-2 "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")


(defn find-badge [[sack-1 sack-2 sack-3]]
  (first (intersection  (set sack-1) (set sack-2) (set sack-3))))

(defn solution-2 []
  (->> (slurp "./resources/puzzle_3.txt")
       (split-lines)
       (partition 3)
       (map find-badge)
       (map item->priority)
       (apply +)))

(comment
  (solution-2)
  ;;=> 2425 !!
  ;;
  )

