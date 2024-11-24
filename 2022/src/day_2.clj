(ns day-2
  (:require [clojure.string :refer [split-lines split]]))

;; https://adventofcode.com/2022/day/2

;; you, me
;;
;; A  , X = Rock     (1)
;; B  , Y = Paper    (2)
;; C  , Z = Scissors (3)

;; X > C    "C" "X"
;; Z > B    "B" "Z"
;; Y > A    "A" "Y"

;; win  (6)
;; draw (3)
;; loww (0)

(defn winner-round? [play]
  (boolean (some #(= play  %) [["C" "X"]
                               ["B" "Z"]
                               ["A" "Y"]])))

(defn draw-round? [[you me]]
  (= (get {"A" "X"
           "B" "Y"
           "C" "Z"} you) me))

(defn outcome-points [play]
  (cond
    (winner-round? play) 6
    (draw-round?   play) 3
    :else                0))

(defn selection-points [[_ me]]
  (get {"X" 1
        "Y" 2
        "Z" 3} me))

(defn read-input []
  (->> (slurp "./resources/puzzle_2.txt")
       (split-lines)
       (map #(split % #" "))))

(defn play->score [play]
  (+ (selection-points play) (outcome-points play)))

(defn solution []
  (->> (read-input)
       (map play->score)
       (apply +)))

(comment
  (solution)
  ;;
  )

;;=> 14069 !!

;; part 2 ----------------------------------------

;; A = Rock     (1)
;; B = Paper    (2)
;; C = Scissors (3)

;; A > C   
;; C > B   
;; B > A 

(defn loosing-shape-points [you]
  (get {"A" 3 ;; "C"
        "B" 1 ;; "A"
        "C" 2 ;; "B"
        }
       you))

(defn winning-shape-points [you]
  (get {"A" 2 ;; "B"
        "B" 3 ;; "C"
        "C" 1 ;; "A"
        }
       you))

(defn draw-shape-points [you]
  (get {"A" 1
        "B" 2
        "C" 3}
       you))


(defn choose-shape [[you my-goal]]
  (case my-goal
    ;; need to loose
    "X" (loosing-shape-points you)

    ;; need to draw
    "Y" (+ 3 (draw-shape-points you))

    ;; need to win
    (+ 6 (winning-shape-points you))))

(defn solution-part-2 []
  (->> (read-input)
       (map choose-shape)
       (apply +)))

;; => 12411

(comment
  (solution-part-2)
  ;;
  )
