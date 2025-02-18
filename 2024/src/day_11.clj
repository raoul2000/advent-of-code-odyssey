(ns day-11
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2024/day/11

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules : 
;; 
;; - If the stone is engraved with the number 0, 
;;         it is replaced by a stone engraved with the number 1.

;; - If the stone is engraved with a number that has an even number of digits, 
;;         it is replaced by two stones. The left half of the digits are engraved on the 
;;         new left stone, and the right half of the digits are engraved on the new right 
;;         stone. (The new numbers don't keep extra leading zeroes: 1000 would become
;;         stones 10 and 0.)

;; - If none of the other rules apply, 
;;         the stone is replaced by a new stone; the old stone's number multiplied by 2024 
;;         is engraved on the new stone.


(def sample-input "125 17")

(def puzzle-input (slurp "resources/day_11.txt"))

(defn has-even-number-of-digit? [s]
  (even? (count s)))

(def engraved-with-zero? (partial = "0"))

(comment
  (has-even-number-of-digit? "1")
  (has-even-number-of-digit? "12")
  (has-even-number-of-digit? "123")

  (engraved-with-zero? "0")
  (engraved-with-zero? "10")
  ;;
  )

(defn split-at-middle [s]
  (->> (partition-all (quot (count s) 2) s)
       (map #(apply str %))
       (map #(Integer/parseInt %))
       (mapv str)))

(comment
  (split-at-middle "1234")
  (split-at-middle "1200")
  (split-at-middle "10")
  ;;
  )

(defn multiply-by-2024 [s]
  (str (* 2024N (biginteger s))))

(comment
  (multiply-by-2024 "22522544002255445566558999880000000000000000000000000")
  ;;
  )

(defn apply-rules [s]
  (cond
    (engraved-with-zero?       s)   ["1"]
    (has-even-number-of-digit? s)   (split-at-middle s)
    :else                           [(multiply-by-2024 s)]))

(defn blink [stones]
  (->> stones
       (map apply-rules)
       flatten))

(defn solution-1 [input blink-count]
  (->> (s/split input #" ")
       (iterate blink)
       (take (inc blink-count))
       last
       count))

(comment

  (solution-1 sample-input 25)
  ;; => 55312 .. good

  (solution-1 puzzle-input 25)
  ;; => 193899 ⭐


  ;;
  )

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - blink 75 times !!

(comment
  (time (solution-1 sample-input 25))
  ; "Elapsed time: 302.4543 msecs"
  (time (solution-1 sample-input 30))
  ; "Elapsed time: 2996.1022 msecs"
  (time (solution-1 sample-input 35))
  ; "Elapsed time: 28380.8791 msecs"

  ;; so, from 0.3s to 3s to 28s ... by a step of +5 💥
  ;; we must optimize

  ;;
  )

(comment

  (->> (s/split "1" #" ")
       (iterate blink)
       (take (inc 5))
       
       )
       
       ;;
       )
