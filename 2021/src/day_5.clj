(ns day-5
  (:require [clojure.string :as str]))

(def test-data "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

;; part 1 =======================

;; ignore : (8,0 -> 0,8) (6,4 -> 2,0) (0,0 -> 8,8) (5,5 -> 8,2)

;; 0 1 2 3 4 5 6 7 8 9
;;------------------------
;; . . . . . . . 1 . . | 0
;; . . 1 . . . . 1 . . | 1
;; . . 1 . . . . 1 . . | 2
;; . . . . . . . 1 . . | 3
;; . 1 1 2 1 1 1 2 1 1 | 4
;; . . . . . . . . . . | 5
;; . . . . . . . . . . | 6
;; . . . . . . . . . . | 7
;; . . . . . . . . . . | 8
;; 2 2 2 1 1 1 . . . . | 9

(defn read-puzzle-4 [s]
  "Returns an seq of vector (pair of points)
   
   Ex: 
   ```
   ( 
     ( (0 9) (5 9) )
     ( (8 0) (0 8) )
     etc ...
   )
   ```" []
  (->> (str/split-lines s)
       (map #(->> (re-matches #"([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)" %)
                  rest
                  (map (fn [s] (Integer/parseInt s)))))
       (map #(partition 2 %))))

(comment
  (read-puzzle-4 test-data)
  (read-puzzle-4 (slurp "./resources/puzzle_5.txt")))

;; considering only horizontal and vertical

(defn vert-or-horiz?
  "Returns *true* if the vector is veritcal or horizontal"
  [[[x1 y1] [x2 y2]]]
  (or (= x1 x2)
      (= y1 y2)))

(comment
  (vert-or-horiz? [[1 2] [3 4]])
  (vert-or-horiz? [[1 2] [1 4]])
  (vert-or-horiz? [[3 2] [5 2]])
  (vert-or-horiz? [[3 2] [3 2]]))

;; create intermediates points
(def v1 [[0 9] [5 9]])
(def v2 [[9 4] [3 4]])
(def v3 [[7 0] [7 4]])

;; let's use (range)

;; [[7 0] [7 4]] dy (x = 7)
(map vector (repeat 7) (range (inc (min 0 4)) (max 0 4)))
;; [[0 9] [5 9]] dx (y = 9)
(map vector  (range (inc (min 0 5)) (max 0 5)) (repeat 9))

(defn create-points
  "Creates an returns a seq of point belonging to a given vector, assumed
   to be vertical or horizontal"
  [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (map list (repeat x2) (range (inc (min y1 y2)) (max y1 y2)))   ;; dy (x = x2)
    (= y1 y2) (map list (range (inc (min x1 x2)) (max x1 x2)) (repeat y2)))) ;; dx (y = y2)

(comment
  (concat v1 (create-points v1))
  (create-points v2)
  (create-points v3))


;; to count duplicated (overlapped) points, let's use (frequencies)
;; to build a map where k is a point and v its frequency in the input seq
(frequencies '([0 9] [5 9] [1 9] [2 9] [3 9] [4 9] [0 9]))

(defn frequencies>1? [[_ freq]]
  (> freq 1))

(defn solve-part1 [s]
  (->> (read-puzzle-4 s)
       (filter vert-or-horiz?)
       (map #(concat % (create-points %)))
       (reduce (fn [result v] (concat result v)) '())
       frequencies
       (filter frequencies>1?)
       count))

(time (solve-part1 test-data))
;; "Elapsed time: 0.2844 msecs"
;; => 5

(time (solve-part1 (slurp "./resources/puzzle_5.txt")))
;; "Elapsed time: 2020.1042 msecs"
;; => 5774


;; part 2 =======================

;; diagonal vector at 45 degrees must now be taken into account
;; ex: (1, 1) => (3, 3)
;;     (0 ,8) => (8, 0)
;; diagonal vectors must satisfy the following assertion :
;; (= (abs (- x1 x2)) (abs (- y1 y2)))

(defn diagonal-vector? [[[x1 y1] [x2 y2]]]
  (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

;; create intermediates point foe diagonal (45 degrees) vector
;; (1, 1) => (3, 3)
(map vector (range 1 3) (range 1 3))

;; (0, 4) (4, 0)
(map vector (range 1 4) (range 1 4))

;; 9,7 -> 7,9
(map vector (range 8 9) (range 8 9))

;; 11,7 -> 7,11
(map vector (range 11 7 -1) (range 7 11))

(defn create-points-diagonal [[[x1 y1] [x2 y2]]]
  (rest (map vector
             (range x1 x2 (if (> x1 x2) -1 1))
             (range y1 y2 (if (> y1 y2) -1 1)))))

(comment
  (create-points-diagonal [[0 9] [9 0]])
  (create-points-diagonal [[1 1] [3 3]])
  (create-points-diagonal [[9 7] [7 9]]))


(defn create-points-vert [[[x1 y1] [_ y2]]]
  (map list (repeat x1) (range (inc (min y1 y2)) (max y1 y2))))

(defn create-points-horiz [[[x1 y1] [x2 _]]]
  (map list (range (inc (min x1 x2)) (max x1 x2)) (repeat y1)))

(defn create-points-2
  "Creates an returns a seq of point belonging to a given vector, assumed
   to be vertical or horizontal or 45 degrees"
  [[[x1 y1] [x2 y2] :as v]]
  (cond
    (diagonal-vector? v) (create-points-diagonal v) ;; dx & dy
    (= x1 x2) (create-points-vert v)                ;; dy (x = x2)
    (= y1 y2) (create-points-horiz v)))             ;; dx (y = y2)

(comment
  (create-points-2 [[0 9] [9 0]]))


(defn solve-part2 [s]
  (->> (read-puzzle-4 s)
       (map #(concat % (create-points-2 %)))
       (reduce (fn [result v] (concat result v)) '())
       frequencies
       (filter frequencies>1?)
       count))

(time (solve-part2 test-data))
;; "Elapsed time: 0.4654 msecs"
;; 12

(time (solve-part2 (slurp "./resources/puzzle_5.txt")))
;; "Elapsed time: 5316.826 msecs"
;; => 18423
