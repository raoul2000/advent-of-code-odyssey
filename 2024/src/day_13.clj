(ns day-13
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

;; https://adventofcode.com/2024/day/13

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; some names : 
;; - push-a : number of times the button A is pushed
;; - dx-a : distance moved on x axis when button A is pushed
;; - prize-x prize-y : coordinates of the prize to win
;;
;; So, For machine 1 we have :
;; (push-a * dx-a) + (push-b * dx-b) = prize-x
;; (push-a * dy-a) + (push-b * dy-b) = prize-y
;;
;; 1. push-a = (prize-x - (push-b * dx-b)) / dx-a
;; 2. push-a = (prize-y - (push-b * dy-b)) / dy-a
;;
;; so we have ...
;; (prize-x - (push-b * dx-b)) / dx-a = (prize-y - (push-b * dy-b)) / dy-a
;; (prize-x - (push-b * dx-b)) * dy-a = (prize-y - (push-b * dy-b)) * dx-a
;; (prize-x * dy-a) - (push-b * dx-b * dy-a)   = (prize-y * dx-a) - (push-b * dy-b * dx-a)
;; (push-b * dx-b * dy-a)   = (prize-y * dx-a) - (push-b * dy-b * dx-a) - (prize-x * dy-a)
;; (push-b * dx-b * dy-a) + (push-b * dy-b * dx-a) = (prize-y * dx-a) - (prize-x * dy-a)
;; push-b * ((dx-b * dy-a) + (dy-b * dx-a)) = (prize-y * dx-a) - (prize-x * dy-a)
;;
;; which give push-b : 
;;  push-b  = ((prize-y * dx-a) - (prize-x * dy-a)) / ((dx-b * dy-a) + (dy-b * dx-a))

;; (/ 
;;     (- (* prize-y dx-a) (* prize-x dy-a)) 
;;     (+ (* dx-b dy-a)    (* dy-b dx-a))
;; )


(def sample-input-1 "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279

")

(comment
  (/
   (- (* 5400 94) (* 8400 34))
   (+ (* 22 34)  (* 67 94)))
  ;; (/ 
;;     (- (* prize-y dx-a) (* prize-x dy-a)) 
;;     (+ (* dx-b dy-a)    (* dy-b dx-a))
;; )
  ;;
  )
(def puzzle-input (slurp "resources/day_13.txt"))

;; the data model
(comment

  (s/split sample-input-1 #"\n\n")
  (def model-machine {:button-A {:dx 94  :dy 34}
                      :button-b {:dx 22  :dy 67}
                      :prize    {:x 8400 :y 5400}})
  (.parseInt Integer "44")
  (edn/read-string "3345")
  ;; let's use regexp
  (re-matches #"Button A: X\+(\d+)" "Button A: X+94")
  (re-matches #"Button A: X\+(\d+), Y\+(\d+)" "Button A: X+94, Y+34")
  (re-matches #"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)" "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=18641, Y=10279")

  ;;
  )

(defn parse-machine-line
  "Given a set of string lines representing the textual representation of a machine, returns
   numerical values describing this machine.
   
   For example : 
   ```
   Button A: X+94, Y+34
   Button B: X+22, Y+67
   Prize: X=8400, Y=5400
   ```
   Returns : `[dx-a dy-a dx-b dy-b prize-x prize-y]` where in the above example would be : 

   - dxa = 94
   - dya = 34
   - dxb = 22 
   - dyb = 67
   - prize-x = 8400
   - prize-y = 5400

   "
  [line]
  (->> line
       (re-matches #"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)")
       (rest)
       (map edn/read-string)))

(defn lines->machine
  "Given 3 lines representing a machine, returns a map describing the machine.
   
   Example input : 
   ```
   Button A: X+94, Y+34
   Button B: X+22, Y+67
   Prize: X=8400, Y=5400
   ```
   "
  [line]
  (let [[dx-a dy-a dx-b dy-b prize-x prize-y] (parse-machine-line line)
        num-push-y        (- (* dx-a prize-y) (* dy-a prize-x))
        denum-pushy       (- (* dy-b dx-a)    (* dx-b dy-a))
        push-y            (when (zero? (rem num-push-y denum-pushy))
                            (quot num-push-y denum-pushy))

        push-x            (when push-y
                            (let [num-push-x   (- prize-x (* push-y dx-b))
                                  denum-push-x  dx-a]
                              (when (zero? (rem num-push-x denum-push-x))
                                (quot num-push-x denum-push-x))))
        cost              (when (and push-y push-x)
                            (+ (* 3 push-x) push-y))]

    {:button-A {:dx dx-a   :dy dy-a}
     :button-b {:dx dx-b   :dy dy-b}
     :prize    {:x prize-x :y prize-y}
     :push-y   push-y
     :push-x   push-x
     :cost     cost}))

(defn solution-1 [s]
  (->> (s/split s #"\n\n")
       (map lines->machine)
       ;; ignore machines with no way to reach prize
       (filter #(and (:push-x %) (:push-y %)))

       ;; ignore machine with more than 100 push
       (filter #(and (>= 100 (:push-x %))
                     (>= 100 (:push-y %))))
       ;; get cost
       (map :cost)

       ;; total cost
       (reduce +)))

(comment
  (solution-1 sample-input-1)
  ;; => 480 ... good !
  (solution-1 puzzle-input)
  ;; => 26599 ⭐

  ;;
  )

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add 10000000000000 to all prize-x and prize-y
;; do not limit to 100 push


(defn lines->machine2
  "Given 3 lines representing a machine, returns a map describing the machine.
   
   Example input : 
   ```
   Button A: X+94, Y+34
   Button B: X+22, Y+67
   Prize: X=8400, Y=5400
   ```
   "
  [line]
  (let [[dx-a dy-a dx-b dy-b prz-x prz-y] (parse-machine-line line)
        prize-x   (+ prz-x 10000000000000N)
        prize-y   (+ prz-y 10000000000000N)
        num-push-y        (- (* dx-a prize-y) (* dy-a prize-x))
        denum-pushy       (- (* dy-b dx-a)    (* dx-b dy-a))
        push-y            (when (zero? (rem num-push-y denum-pushy))
                            (quot num-push-y denum-pushy))

        push-x            (when push-y
                            (let [num-push-x   (- prize-x (* push-y dx-b))
                                  denum-push-x  dx-a]
                              (when (zero? (rem num-push-x denum-push-x))
                                (quot num-push-x denum-push-x))))
        cost              (when (and push-y push-x)
                            (+ (* 3 push-x) push-y))]

    {:button-A {:dx dx-a   :dy dy-a}
     :button-b {:dx dx-b   :dy dy-b}
     :prize    {:x prize-x :y prize-y}
     :push-y   push-y
     :push-x   push-x
     :cost     cost}))

(defn solution-2 [s]
  (->> (s/split s #"\n\n")
       (map lines->machine2)
       ;; ignore machines with no way to reach prize
       (filter #(and (:push-x %) (:push-y %)))


       ;; get cost
       (map :cost)

       ;; total cost
       (reduce +)))

(comment

  (solution-2 puzzle-input)
  ;; => 106228669504887 yes !!⭐
  ;;
  )

