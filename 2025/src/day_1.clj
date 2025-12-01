(ns day-1
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2025/day/1

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (your puzzle input) contains a sequence of rotations
;; a rotation = direction (L, R)  distance = *n*

;; around the dial are the numbers 0 through 99 in order
;; Because the dial is a circle, turning the dial left from 0 one click makes it point at 99. 
;; Similarly, turning the dial right from 99 one click makes it point at 0
;; The dial starts by pointing at 50

;; Because the dial points at 0 a total of three times during this process, 
;; the password in this example is 3


(def sample-input "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
")

(def puzzle-input (slurp "resources/day_1.txt"))

(defn parse-input
  "Returns a seq of rotations where as pairs where first is the direction and
   second is the distance as integer."
  [input]
  (->> (s/split-lines input)
       (map #(->
              (re-matches #"(.)(.+)" %)
              ((juxt second (fn [s] (-> s last Integer/parseInt))))))))

(defn apply-rotation 
  "Compute and returns the new dial value."
  [[direction distance] dial-num]
  (if (zero? distance)
    dial-num ;; don't move (stupid ?)
    (mod ((if (= "L" direction) - +) dial-num distance) 100)))


(defn solution-1 [input]
  (->> (reduce (fn [[zero-count, dial-num], rotation]
                 (let [new-dial (apply-rotation rotation dial-num)]
                   [((if (zero? new-dial) inc identity)  zero-count)
                    new-dial])) [0 50] (parse-input input))
       first))

(comment

  (solution-1 sample-input)
  ;; => 3 ... looking good

  (solution-1 puzzle-input)
  ;; => 1102 ⭐
  ;;
  )

