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

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; you're actually supposed to count the number of times any click causes the dial to point at 0, regardless of whether 
;; it happens during a rotation or at the end of one.

;; When does a rotation crosses a zero ? 
;; => when the distance is greater then the distance between the current dial and the position of zero in the direction
;;    specified by the rotation
;; => when the distance is greater than 99

(defn cross-zero-count [[direction distance] dial-num]
  (if (zero? distance)
    0
    (let [distance-to-zero (if (= "L" direction) dial-num (- 100 dial-num))]
      (if (>=  distance-to-zero distance)
        0
        (let [full-loop-count (quot distance 100)
              remain-distance (mod distance 100)
              ]
          (+ (* full-loop-count 1)
             
             )
          )
        1))))

(comment
  (quot 50 100)
  (quot 100 100)
  (quot 120 100)
  (quot 220 100)
  (mod 100 100)
  (mod 150 100)
  ;;
  )

(comment
  (defn dial)
  ;;
  )

