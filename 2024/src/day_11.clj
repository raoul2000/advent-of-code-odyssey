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
;; stone = [1]
;; initial state 
;; {1     { :count 1
;;          :next [2024]}}
;;
  (def initial-state {1 {:count 1 :next [2024]}})
  ;; => 1
  ;; after one blink
  (def blink-1 {1    {:count 0 :next [2024]}
                2024 {:count 1 :next [20 24]}})
  ;; => 2024

  ;; blink 2 ----------------------------
  (def blink-2 {1    {:count 0 :next [2024]}
                2024 {:count 0 :next [20 24]}
                20   {:count 1 :next [2 0]}
                24   {:count 1 :next [2 4]}})
  ;; => 20 24

  ;; blink 3 ----------------------------
  (def blink-3 {1    {:count 0 :next [2024]}
                2024 {:count 0 :next [20 24]}
                20   {:count 0 :next [2 0]}
                24   {:count 0 :next [2 4]}
                2    {:count 2 :next [4048]}
                0    {:count 1 :next [1]}
                4    {:count 1 :next [8096]}})
  ;; => 2 0 2 4

  ; blink 4 -------------------------------
  (def blink-4 {1    {:count 1 :next [2024]}
                2024 {:count 0 :next [20 24]}
                20   {:count 0 :next [2 0]}
                24   {:count 0 :next [2 4]}
                2    {:count 0 :next [4048]}
                0    {:count 0 :next [1]}
                4    {:count 0 :next [8096]}
                4048 {:count 2 :next [40 48]}
                8096 {:count 1 :next [80 96]}})
  ;; => 4048 1 4048 8096

  ;; blink 5 ---------------------------------
  (def blink-5 {1    {:count 0 :next [2024]}
                2024 {:count 1 :next [20 24]}
                20   {:count 0 :next [2 0]}
                24   {:count 0 :next [2 4]}
                2    {:count 0 :next [4048]}
                0    {:count 0 :next [1]}
                4    {:count 0 :next [8096]}
                4048 {:count 0 :next [40 48]}
                8096 {:count 0 :next [80 96]}
                40   {:count 2 :next [4 0]}
                48   {:count 2 :next [4 8]}
                80   {:count 1 :next [8 0]}
                96   {:count 1 :next [9 6]}})
  ; => 40 48 2024 40 48 80 96

  ;; blink 6 -------------------------------
  (def blink-6 {1    {:count 0 :next [2024]}
                2024 {:count 0 :next [20 24]}
                20   {:count 1 :next [2 0]}
                24   {:count 1 :next [2 4]}
                2    {:count 0 :next [4048]}
                0    {:count 3 :next [1]}
                4    {:count 4 :next [8096]}
                4048 {:count 0 :next [40 48]}
                8096 {:count 0 :next [80 96]}
                40   {:count 0 :next [4 0]}
                48   {:count 0 :next [4 8]}
                80   {:count 0 :next [8 0]}
                96   {:count 0 :next [9 6]}
                8    {:count 3 :next [16192]}
                9    {:count 1 :next [18216]}
                6    {:count 1 :next [12144]}})
  ;; => 4 0 4 8 20 24 4 0 4 8 8 0 9 6

  ;; blink 7 --------------------------------
  (def blink-7 {1    {:count 3 :next [2024]}
                2024 {:count 0 :next [20 24]}
                20   {:count 0 :next [2 0]}
                24   {:count 0 :next [2 4]}
                2    {:count 2 :next [4048]}
                0    {:count 1 :next [1]}
                4    {:count 1 :next [8096]}
                4048 {:count 0 :next [40 48]}
                8096 {:count 4 :next [80 96]}
                40   {:count 0 :next [4 0]}
                48   {:count 0 :next [4 8]}
                80   {:count 0 :next [8 0]}
                96   {:count 0 :next [9 6]}
                8    {:count 0 :next [16192]}
                9    {:count 0 :next [18216]}
                6    {:count 0 :next [12144]}
                16192 {:count 3 :next [2772608]}
                18216 {:count 1 :next [36869184]}
                12144 {:count 1 :next [24579456]}})
  ;; => 8096 1 8096 16192 2 0 2 4 8096 1 8096 16192 16192 1 18216 12144
  
  ;; why not remove all :count = 0 beteween each blink ?


  (->> (s/split "1" #" ")
       (iterate blink)
       (take (inc 5)))

       ;;
  )
