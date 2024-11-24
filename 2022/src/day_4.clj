(ns day-4
  (:require [clojure.string :refer [split-lines split]]
            [clojure.set :refer [intersection]]))

;; https://adventofcode.com/2022/day/4

;; Every section has a unique ID number
;; and each Elf is assigned a range of section IDs.
;; many of the assignments overlap
;; Elves pair up and make a big list of the section assignments for each pair (your puzzle input)

;; In how many assignment pairs does one range fully contain the other?

(def sample "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn pairs->ranges
  "Given pair range *s*, return a vector of 2 vectors, each one
   representing an int range
   
   ex: 1-3,4-6 => [[1 2 3] [4 5 6]]"
  [s]
  (let [[p1-start p1-end p2-start p2-end] (map #(Integer/parseInt % 10) (split s #"[,-]"))]
    (vector (set (range p1-start (inc p1-end)))
            (set (range p2-start (inc p2-end))))))

(comment
  (split "2-4,5-10" #"[,-]")
  (Integer/parseInt "5" 10)
  (pairs->ranges "1-4,7,10")
  (range 1 2)
  ;;
  )

(defn fully-contains? [[r1 r2]]
  (or (= r1 (intersection r1 r2))
      (= r2 (intersection r2 r1))))

(comment
  (fully-contains? [#{4 3 2} #{7 6 8}])
  (fully-contains?  [#{7 4 6 3 2 5 8} #{7 4 6 3 5}])
  (fully-contains?  [ #{7 4 6 3 5} #{7 4 6 3 2 5 8}])
  ;;
  )

(defn solution-1 []
  (->> (slurp "./resources/puzzle_4.txt")
       (split-lines)
       (map pairs->ranges)
       (filter fully-contains?)
       (count)))

(comment
  (solution-1)
  ;;=> 433 !!
  ;;
  )


;; part 2 ----------------------------------------------------------

;; the Elves would like to know the number of pairs that overlap at all.

(defn range-overlap? [[r1 r2]]
  (boolean (seq (intersection r1 r2))))

(comment
  (range-overlap? [#{4 7 2} #{7 6 8}])
  (range-overlap? [#{4 1 2} #{7 6 8}])
  ;;
  )

(defn solution-2 []
  (->> (slurp "./resources/puzzle_4.txt")
       (split-lines)
       (map pairs->ranges)
       (filter range-overlap?)
       (count)))

(comment
  (solution-2)
  ;;=> 852 !!
  )