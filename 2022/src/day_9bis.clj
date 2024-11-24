(ns day-9bis
  (:require [clojure.string :refer [split-lines split]]))

;; https://adventofcode.com/2022/day/9

;; - part 1 -------------------------------------------

;; all steps performed by the head will be also performed by the tail
;; When the head moves in diagonal, it remains adjacent to the tailn so the
;; tail does not move but 'remember' the move for later, 

(def sample "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
")

(defn input->steps
  "Turns puzzle input into a seq of directed steps. For example
   'U 4' is turned into ['U' 'U' 'U' 'U']."
  [s]
  (reduce (fn [acc motion]
            (let [[dir steps] (split motion #" ")]
              (into acc (repeat (Integer/parseInt steps) dir)))) [] (split-lines s)))

(comment
  (input->steps sample)
  ;;
  )

(defn move-one-step
  "Move coordinates *[x y]* one unit in *direction* and returns the new position"
  [[x y] direction]
  (case direction
    "U" [x (dec y)]
    "D" [x (inc y)]
    "L" [(dec x) y]
    "R" [(inc x) y]))


(defn move-steps
  "Given an *initial-pos* and a coll of steps, returns the updated pos
   after steps are performed starting from *initial-pos*"
  [initial-pos step-coll]
  (reduce move-one-step initial-pos step-coll))

(comment
  (move-steps [0 0] ["U" "D" "L" "R"])
  ;;
  )

(defn adjacent-pos?
  "Returns TRUE if position *[x1 y1]* and *[x2 y2]* are adjacent position
   or the same position (overlapping)"
  [[x1 y1] [x2 y2]]
  (let [adjacent-positions (for [dx [-1 0 1]
                                 dy [-1 0 1]]
                             [(+ dx x1) (+ dy y1)])]
    (some #{[x2 y2]} adjacent-positions)))

(comment
  (adjacent-pos? [1 1] [1 1])
  (adjacent-pos? [1 1] [2 1])
  (adjacent-pos? [1 1] [2 2])
  ;;
  )

(defn reach-pos [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (get {[2  0] [(inc x1) y1]
          [-2 0] [(dec x1) y1]
          [0  2] [x1 (inc y1)]
          [0 -2] [x1 (dec y1)]
          ;; top right
          [1  2]  [(inc x1) (inc y1)]
          [2  1]  [(inc x1) (inc y1)]
          [2  2]  [(inc x1) (inc y1)]
          ;; top left
          [-1  2]  [(dec x1) (inc y1)]
          [-2  1]  [(dec x1) (inc y1)]
          [-2  2]  [(dec x1) (inc y1)]
          ;; bottom right
          [2 -1]  [(inc x1) (dec y1)]
          [1 -2]  [(inc x1) (dec y1)]
          [2 -2]  [(inc x1) (dec y1)]
          ;; bottom left
          [-2 -1] [(dec x1) (dec y1)]
          [-1 -2] [(dec x1) (dec y1)]
          [-2 -2] [(dec x1) (dec y1)]} [dx dy])))

(comment

  (reach-pos [0 0] [2 0])
  (reach-pos [0 0] [-2 0])
  (reach-pos [0 0] [-1 -2])
  ;;
  )


(defn solution-1 []
  (loop [steps            (input->steps
                           ;;sample
                           (slurp "./resources/puzzle_9.txt")
                           )
         head-pos         [0 0]
         tail-pos         [0 0]
         tail-pos-history []]
    (if (empty? steps)
      tail-pos-history
      (let [step                             (first steps)
            new-head-pos                     (move-one-step head-pos step)
            new-tail-pos  (if-not (adjacent-pos? new-head-pos tail-pos)
                            (reach-pos tail-pos new-head-pos)
                            tail-pos)]
        (recur (rest steps)
               new-head-pos
               new-tail-pos
               (conj tail-pos-history new-tail-pos))))))
(comment

  (count (into #{} (solution-1)))
  ;; => 6030 !!
  )

;; ---- part 2 ------------------------------------------------

;; Rather than two knots, you now must simulate a rope consisting of ten knots.
;; Now, you need to keep track of the positions the new tail, 9, visits

(defn next-knot-path [pos-coll]
  (loop [head-pos-coll pos-coll
         tail-pos-history [[0 0]]]
    (if (empty? head-pos-coll)
      tail-pos-history
      (let [head-pos (first head-pos-coll)
            tail-pos (last tail-pos-history)
            new-tail-pos (if-not (adjacent-pos? tail-pos head-pos)
                           (reach-pos tail-pos head-pos)
                           tail-pos)]
        (recur (rest head-pos-coll)
               (conj tail-pos-history new-tail-pos))))))

(defn solution-2 []
  (let [head-path (solution-1)]
    (->> (iterate next-knot-path head-path)
         (take 9)
         (last)
         (into #{})
         (count))))



(comment
  (solution-2)
  ;; => 25454 !!

  )