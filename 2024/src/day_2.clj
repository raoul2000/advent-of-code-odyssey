(ns day-2
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2024/day/2/input

;; one 'report' per line
;; one line is a list of 'levels'  each level is a positive integer
;; whcih reports are 'safe' ?
;; a reportis safe if :
;; 
;; - The levels are either all increasing or all decreasing.
;; ... AND ...
;; - Any two adjacent levels differ by at least one and at most three.
;;
;; Q1: How many reports are safe?

(comment

  ;; all increasing ?
  (apply < [1 2 3 3])
  (apply < [1 2 12 5])
  (apply < [1 2 3 4])

  ;;
  )

(def all-increasing? (partial apply <))
(def all-decreasing? (partial apply >))

(comment
  ;;level diff
  (def report [1 3 6 10])

  (map - report (next report))
  ;; valid level diff ?
  (< -4 0 4)
  ;;
  )

(defn compute-level-diff [report]
  (map - report (next report)))

(defn valid-level-diff? [n]
  (< -4 n 4))

;; process input data
(def sample-data "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn read-input [s]
  (->> (s/split-lines s)
       (map #(s/split % #" "))
       (map (fn [report]
              (mapv #(Integer/parseInt %) report)))))

(defn solution-1 []
  (->> (read-input (slurp "resources/day_2.txt"))
       (filter #(or (all-decreasing? %)
                    (all-increasing? %)))
       (map compute-level-diff)
       (filter #(every? valid-level-diff?  %))
       count))

(solution-1)
;; => 236 ⭐


;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; an unsafe report can be considered as safe if it meets safety requirement
;; by removing one level

(defn make-derived-report [report]
  (map-indexed (fn [i _n]
                 (keep-indexed #(when-not (= i %1) %2) report)) report))

(defn strictly-safe [report]
  (and (or (all-increasing? report)
           (all-decreasing? report))
       (->> report
            compute-level-diff
            (every? valid-level-diff?))))

(defn safe-report-with-tolerance [report]
  (or (strictly-safe report)
      (some strictly-safe (make-derived-report report))))

(defn solution-2 []
  (->> (slurp "resources/day_2.txt")
       read-input
       (filter safe-report-with-tolerance)
       count))

(solution-2)
;; => 308 ⭐