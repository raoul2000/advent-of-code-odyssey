(ns day-6
  (:require [clojure.string :as s]))


;; https://adventofcode.com/2024/day/6

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - '^' : current position of the guard (facing up)
;; - '#' : obstruction

;; guard moving rules
;; 
;; - If there is something directly in front of you, turn right 90 degrees.
;; - Otherwise, take a step forward.
;; and then leaves the map

(def sample-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")
(def puzzle-input (slurp "resources/day_6.txt"))

;; let's create data structure for the grid

(defn create-grid [s]
  (->> (s/split-lines s)
       (mapv vec)))

;; and re-use some utils function for grid management (see day_4)

(defn char-at [grid [x y]]
  (get-in grid [y x]))

(defn grid-size
  "Returns [col-count line-count] for the given grid"
  [grid]
  [(count (first grid)) (count grid)])

(defn in-grid?
  "Return TRUE if pos [x y] is inside the given `grid`"
  [grid [x y]]
  (let [[col-count line-count] (grid-size grid)]
    (and (< -1 x col-count)
         (< -1 y line-count))))

(defn starting-pos [grid]
  (let [[col-count line-count] (grid-size grid)]
    (first (drop-while #(not= \^ (char-at grid %)) (for [x (range 0 col-count)
                                                         y (range 0 line-count)]
                                                     [x y])))))

(defn move [current-pos direction]
  (mapv + current-pos direction))

(comment
  (move [0 0] [0 1])
  (char-at (create-grid sample-input) [1 1])
  (starting-pos (create-grid sample-input))
  ;;
  )

(defn rotate-direction-90 [[dx dy]]
  (case [dx dy]
    [0 -1]  [1  0]
    [1  0]  [0  1]
    [0  1]  [-1 0]
    [-1 0]  [0 -1]))


(defn scan-pos
  "Returns the char in grid, at `current-pos` + `direction`or *nil*
   when out of grid."
  [current-pos direction grid]
  (char-at grid (map + current-pos direction)))

(comment
  (scan-pos [0 0] [0 1] (create-grid sample-input))
  ;;
  )

(defn find-next-move [current-pos current-dir grid]
  (loop [new-dir current-dir]
    (let [next-pos (move new-dir current-pos)]
      (if (not= \# (char-at grid next-pos))
        {:pos next-pos
         :dir new-dir}
        (recur (rotate-direction-90 current-dir))))))

(comment
  (def grid (create-grid sample-input))
  (find-next-move (starting-pos grid) [0 -1] grid)
  (find-next-move [4 1] [0 -1] grid)
  (find-next-move [0 0] [0 -1] grid)
  ;; 
  )

(defn build-path [sample-input]

  (let [grid      (create-grid sample-input)
        start-pos (starting-pos grid)]
    (loop [path {:pos  [start-pos]
                 :dir  [[0 -1]]
                 :grid grid}]
      (let [cur-pos (last (:pos path))
            cur-dir (last (:dir path))
            grid    (:grid path)]
        (if-not (in-grid? grid cur-pos)
          path
          (let [{next-pos :pos
                 next-dir :dir} (find-next-move cur-pos cur-dir grid)]
            (recur (-> path
                       (update :pos conj next-pos)
                       (update :dir conj next-dir)))))))))

(defn solution-1 [input]
  (->> input
       build-path
       :pos
       ;; remove last pos which is out of grid
       butlast
       ;; we need only distinct pos
       set
       count))

(solution-1 puzzle-input)
;; => 5312 ⭐


;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
