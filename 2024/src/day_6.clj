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
        (recur (rotate-direction-90 new-dir))))))

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

(comment
  (solution-1 puzzle-input)
  ;; => 5312 ⭐

  (time (solution-1 puzzle-input))
  ;; Elapsed time: 1027.4888 msecs (not good)

  ;;
  )


;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find all of the possible positions for such an obstruction that would get the guard stuck in a loop.
;;
;; Option 1 : 
;; on each position, test if adding an obstruction in the direction will redirect the guard in a position + direction
;; that we already went through.

;; let's try something (not sure if it will not blow up process time)
;; - create the guard path that leaved the grid (the one from solution-1)
;; - for each [position, direction] 
;;     - if the next pos is empty
;;          - modify the grid by adding an obstruction in thie next position
;;          - calculate the new path in this new grid until
;;               - the path leaves the grid 
;;                 OR
;;               - the path is a loop (TODO: loop detection) : in this case, store the position
;;       else 
;;          - continue with the next pos

(defn loop?
  "Returns TRUE if the given `step` exists in the given `path-xs` where :
     
     - a step is a pair like [[x y] [dx dy]]
     - `path-xs` is a collection of steps
     
     "
  [step path-xs]
  (boolean (->> path-xs
                (map #{step})
                (filter identity)
                seq)))

(defn add-obstruction
  "Returns `grid` where an obstruction character is added at position `[x y]`.
   Returns `grid`with no change when position is out of grid.
   "
  [[x y] grid]
  (let [[col-count line-count] (grid-size grid)]
    (if (and (< -1 x col-count)
             (< -1 y line-count))
      (update-in grid [y x] (constantly \#))
      grid)))


(def step-position first)
(def step-direction second)

(defn build-obstructed-path
  "Returns a path starting from `start-step` in `initial-grid` where an 
   obstruction is added to position `obstruction-pos`.

   Returns **nil** when a loop is detected."
  [initial-grid start-step obstruction-pos]
  (let [grid      (add-obstruction obstruction-pos initial-grid)]
    (loop [steps [start-step]]
      (let [cur-pos (step-position  (last steps))
            cur-dir (step-direction (last steps))]
        (cond
          ;; guard left the grid
          (not (in-grid? grid cur-pos))            (butlast steps)

          ;; guard enters in a loop
          (loop? (last steps) (butlast steps))     nil

          ;; continue with next tep
          :else
          (let [{next-pos :pos
                 next-dir :dir} (find-next-move cur-pos cur-dir grid)]
            (recur (conj steps [next-pos next-dir]))))))))

;; this is not required because obstruction pos is in fact the list of pos of the output steps
;; except the first one (starting pos)
(defn next-obstruction-pos
  "Returns the position of the next obstruction point from the current step `[pos dir]`
   in the given `grid`.
   
   Returns **nil** if no obstruction pos could be found.
   "
  [grid [pos dir]]
  (let [obstruction-pos (mapv + pos dir)
        char-at-obstruction-pos (char-at grid obstruction-pos)]
    (when (= \. char-at-obstruction-pos)
      obstruction-pos)))


(comment
  (= (build-obstructed-path (create-grid sample-input)  [[4 6] [0 -1]] [4 0])
     (build-obstructed-path (create-grid sample-input)  [[4 6] [0 -1]] [-1 -1])))

(defn solution-2-mapper [obstruction-pos]
  (println obstruction-pos)
  (Thread/sleep 1)
  (when (nil? (build-obstructed-path grid first-step obstruction-pos))
    obstruction-pos))

(defn solution-2 [input]
  (let [grid               (create-grid input)
        first-step         [(starting-pos grid) [0 -1]]
        reference-steps    (build-obstructed-path grid first-step [-1 -1])
        obstruction-pos-xs (rest (map first reference-steps))]
    (doall (map (fn [obstruction-pos]
                  (when (nil? (build-obstructed-path grid first-step obstruction-pos))
                    obstruction-pos))
                obstruction-pos-xs))))

(comment
  (time (solution-2 sample-input))

  (->> (remove nil? (solution-2 sample-input))
       set
       count)
  ;; => 6 👍 
  ;; ok for the sample input but ..



  (->> (remove nil? (solution-2 puzzle-input))
       set
       count)
  ;; when dzaling with the puzzle input, this BRUTE FORCE solution 
  ;; just explodes 💥 .. process time is infinite 😭


  (printf "hello")
  ;;
  )
