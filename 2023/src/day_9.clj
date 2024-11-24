(ns day-9
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/9

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each line in the input are sensor history values

(def sample-input "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
")

;; Part 1 consist in :
;; - reducing each history line into a list of values until all values are zero
;; - computing from last to first list, the next value in the history seq

;; First : parsing
(defn parse-sensor-history [input]
  (->> (s/split-lines input)
       (mapv (fn [line]
               (mapv #(Integer/parseInt %) (s/split line #" "))))))

(comment
  (parse-sensor-history sample-input)
  ;;
  )

;; detect a stop condition : seq of zero
(defn done? [xs]
  (every? zero? xs))

(comment
  (done? [1 2 3])
  (done? [1 0 0])
  (done? [0 0 0])
  ;;
  )

;; Better check the opposite condition
(defn not-done? [xs]
  (some pos-int? xs))

(comment
  (not-done? [0 0 0 1])
  (not-done? [0 0 0 0])
  ;;
  )

(comment
  ;; at last, derivate the history values
  (defn derive-history [xs]
    (mapv (fn [[a b]] (- b a)) (partition 2 1 xs)))

  (def result (take-while not-done? (iterate derive-history [10  13  16  21  30  45])))

  ;; Eventually reduce the previous result
  (reduce (fn [acc xs]
            (+ acc (last xs))) 0 (reverse result))

  ;; Let's create function for that 
  )

(defn derive-history [xs]
  (mapv (fn [[a b]] (- b a)) (partition 2 1 xs)))

(defn deltas [xs]
  (take-while not-done? (iterate derive-history xs)))

(defn prediction [sensor-history]
  (let [derived-values (deltas sensor-history)]
    (reduce (fn [acc xs]
              (+ acc (last xs))) 0 (reverse derived-values))))

(comment
  (prediction [10  13  16  21  30  45])
  ;;
  )

(defn solution-1 [input]
  (->> input
       parse-sensor-history
       (map prediction)
       (reduce +)))

(comment
  (solution-1 sample-input)
  ;; => 114 This is the expected result with sample input.

  ;; let's try now with puzzle inputs
  (solution-1 (slurp "resources/day_9.txt"))
  ;; => 1708216566 😠 .. what ? "Too high" ? I was sure I was going to get a new star
  ;;

  ;; Maybe there's some error related to negatives values ? 
  ;; let's decompose the first line of puzzle input
  (deltas [4 7 14 17 8 -3 40 284 1054 3047 7774 18514 42224 93097 198790 410780 820904 1587039 2972406 5405827 9575760])
  ;; last is  [5 5 5 5 5 5] which will be turned into [0 0 0 0 0] (end)
  ;; This weems ok.

  ;; Do we have some double spaces as sperator in the input data ? 
  ;; With the current regexp, this could cause empty string item
  (s/split "1  2 3  6" #" ")

  ;; let's check
  (->> (s/split-lines (slurp "resources/day_9.txt"))
       (map #(s/split % #" "))
       (map #(some #{""} %))
       (remove nil?))
  ;; nope !

  ;; trying with input found on reddit (https://www.reddit.com/r/adventofcode/comments/18qx7wy/2023_day_9_part_1_java_why_does_my_solution_not/)
  ;; expected anwser is 64
  (solution-1 "-27 -8 -1 0 1 8 27")
  ;; => 64 it is

  ;; Try with another input
  (deltas [15 30 63 128 258 533 1138 2479 5409 11662 24677 51151 103958 207625 408542 793752 1524859 2897750 5445011 10106818 18508538])
  (solution-1 "15 30 63 128 258 533 1138 2479 5409 11662 24677 51151 103958 207625 408542 793752 1524859 2897750 5445011 10106818 18508538")
  ;; => 33400277
  ;; Representing this in a graph view (Excel) seems to validate the prediction

  ;; Until now I have no clue with my anwser is wrong.

  ;; I will try with a clojure solution found on reddit
  ;; https://www.reddit.com/r/Clojure/comments/18ek2ei/advent_of_code_day_9/

  ;;
  )
(defn parse-seq [s]
  (->> (s/split s #"\s+")
       (map #(Integer/parseInt %))))

(defn element-diffs [seq]
  (->> seq
       (partition 2 1)
       (map reverse)
       (map (partial apply -))))

(defn generate-next [seq]
  (->> seq
       (iterate element-diffs)
       (take-while not-empty)
       (map last)
       (apply +)))

(defn part1 [input]
  (->> input
       (s/split-lines)
       (map parse-seq)
       (map generate-next)
       (apply +)))

(comment
  (part1 sample-input)
  (part1 (slurp "resources/day_9.txt"))
  ;; => 1708206096
  ;; which is not the result I found (1708216566)
  ;; There is a diff of 10470

  (def seq-1 [15 30 63 128 258 533 1138 2479 5409 11662 24677 51151 103958 207625 408542 793752 1524859 2897750 5445011 10106818 18508538])
  (element-diffs  seq-1)
  (= (prediction seq-1) (generate-next  seq-1))

  ;; Let's check if one prediction result differs from the solution found in reddit

  (->> (slurp "resources/day_9.txt")
       (s/split-lines)
       (map parse-seq)
       (map #(vector (- (generate-next %) (prediction %)) %))
       #_(map #(if (zero? (- (generate-next %) (prediction %)))
                 0
                 %))
       (remove (comp zero? first)))
  ;; Wow there are indeed several differences with the expected result
  ;; For example :

  (def seq-2 '(20 40 75 133 225 363 558 818 1146 1538 1981 2451 2911 3309 3576 3624 3344 2604 1247 -911 -4083))
  (prediction seq-2) ;; => -5811
  (generate-next seq-2);; => -8513

  ;; where does this diff comes from ? 
  (deltas seq-2)
  ;; => [3 1 -1 -3 -5 -7 -9 -11 -13 -15 -17 -19 -21 -23 -25 -27 -29]

  ;; NOOOOOOOOO !! ! 😡😡😡

  ;; I found it : the not-done? function is
  (defn not-done? [xs]
    (some pos-int? xs))

  ;; but it is WRONG WRONG WRONG !! it should test for zero values, not pos-int !!
  ;; (I can't believe I did this error 😳 ... sorry)

  ;; ok, let's got ahead  
  )

(defn not-done-1? [xs]
  (some (complement zero?) xs))

(comment
  (not-done-1? [-1 0 0])
  (not-done-1? [0 0 1])
  (not-done-1? [0 0 0])
  ;;
  )

(defn deltas-1 [xs]
  (take-while not-done-1? (iterate derive-history xs)))

(defn prediction-1 [sensor-history]
  (let [derived-values (deltas-1 sensor-history)]
    (reduce (fn [acc xs]
              (+ acc (last xs))) 0 (reverse derived-values))))

(comment
  (prediction-1 [10  13  16  21  30  45])
  ;;
  )

(defn solution-1-a [input]
  (->> input
       parse-sensor-history
       (map prediction-1)
       (reduce +)))

(comment
  ;; Trying one more time
  (solution-1-a sample-input)
    ;; => 114 This is the expected result with sample input.

    ;; let's try now with puzzle inputs
  (solution-1-a (slurp "resources/day_9.txt"))
    ;; => 1708206096 ⭐ (but I don't deserve it 😞)
    ;;
  )

;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Extrapolating first value

;; Could we just revert each history values and apply same functions as part 1
;; Let's try

(defn solution-2 [input]
  (->> input
       parse-sensor-history
       (map reverse)
       (map prediction-1)
       (reduce +)))

(comment
  
  (solution-2 sample-input)
  ;; => 2 oke for sample input

  (solution-2 (slurp "resources/day_9.txt"))
  ;; => 1050 ⭐ good.

  ;; Let's continue to day 10 and forget about day 9
  )


