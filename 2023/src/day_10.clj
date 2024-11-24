(ns day-10
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/10

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We'll have to navigate into a 2x2 grid just like on day 3, so we may reuse
;; function from there.
(def sample-input-1
  "-L|F7
7S-7|
L|7||
-L-J|
L|-JF
")


(def sample-input-2
  "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
")


;; At each position in the grid, there is a char. It can be a :
;; - 'S' : the start position hiding a pipe char
;; - a pipe char
;; - '.' : the ground.

;; Some Adjacent pipe chars can connect with eahc others We must represent this
;; data

(def pipe-connectors {\|  #{:north :south}
                      \-  #{:west  :east}
                      \L  #{:north :east}
                      \J  #{:north :west}
                      \7  #{:south :west}
                      \F  #{:south :east}
                      \S  #{:north :south :east :west}})

;; Given a position with a pipe in it. It can connect in a directions only 
;; if in this direction there is a pipe that can connect to the opposite direction
;; For example:
;; - | can connect :north and :south if
;;    - in the position at :north, there is a pipe that can connect :south
;;    - in the position at :south there is a pipe that can connect :north
;;
;; So what is important is :
;; - the pipe at a given position (x,y)
;; - the pipe at a given direction relatively to pipe at (x,y)

(defn complement-direction [direction]
  (case direction
    :north   :south
    :south   :north
    :east    :west
    :west    :east
    (throw (Exception. (format "invalid direction : %s" direction)))))

(comment
  (complement-direction :north)
  (complement-direction :north-west)
  ;;
  )

(defn find-matching-pipes
  "Given a *pipe* char and a *direction* returns the set of pipes that
   could connect or nil if there is none.
   Note that \\S is never returned as it is not considered as a pipe part to connect to.
   It is only a starting point that hide any pipe."
  [pipe direction]
  (let [connectors (get pipe-connectors pipe)]
    (when-let [connection-from  (connectors direction)]
      (let [connection-to (complement-direction connection-from)]
        (->> pipe-connectors
             (filter (fn [[candidate-pipe candidate-connectors]]
                       (when-not (= \S candidate-pipe)
                         (candidate-connectors connection-to))))
             (map first)
             (set))))))

(def matching-pipes (memoize find-matching-pipes))

(comment
  (find-matching-pipes \| :north)
  ;; => #{\| \7 \F}
  (find-matching-pipes \| :west)
  ;; => nil
  (find-matching-pipes \- :west)
  ;; => #{\- \L}
  (find-matching-pipes \- :east)
  ;; => #{\- \J \7 \F}
  (find-matching-pipes \- :south)
  ;; => nil
  (matching-pipes \- :west)
  (matching-pipes \S :west)

  ;;
  )

;; Given the initial position S, that is connected to the loop, explore the complete
;; loop and returns steps count to reach thefarthest pipe.

(defn input->grid
  [s]
  (->> s
       (s/split-lines)
       (mapv vec)))

(defn grid-dimensions
  "Given a grid, returns a map describing grid dimensions in terms of col and row count"
  [grid]
  {:col-count (count (first grid))
   :row-count (count grid)})

(defn create-grid [s]
  (let [grid (input->grid s)]
    (merge {:matrix grid} (grid-dimensions grid))))


(comment
  (create-grid sample-input-1)
  (create-grid sample-input-2)
  ;;
  )

(defn in-grid [{:keys [col-count row-count]}]
  (fn [[x y]]
    (and (<= 0 x (dec col-count))
         (<= 0 y (dec row-count)))))

(defn char-at [[x y] grid]
  (-> (:matrix grid)
      (nth y)
      (nth x)))

;; Allowed moves are only : up down left right
(defn adjacent-coords
  "Given a *grid* and a *[x y]* position in this grid, returns 4 triplets, each one describing 
   a tile adjacent to the position.
   - first : **direction** from the current position to the tile
   - second : [x y] **position** of the tile in the grid
   - third : **char** in the tile
   "
  [[x y] grid]
  (let [in-grid? (in-grid grid)]
    (->> (vector [:east  [(inc x)  y]]
                 [:west  [(dec x)  y]]
                 [:south [x  (inc y)]]
                 [:north [x  (dec y)]])
         (filterv (comp in-grid? second))
         (map #(conj % (char-at (second %) grid))))))


(comment
  (def grid-1 (create-grid sample-input-1))
  (adjacent-coords [1 1] grid-1)
  (char-at [1 2] grid-1)
  ;;
  )


;; By the way, we need a function to find S, the start position:
(defn find-S-pos
  "Returns position [x, y] of the first \\S char in grid or nil."
  [grid]
  (let [all-coords (for [x (range 0 (:col-count grid))
                         y (range 0 (:row-count grid))]
                     [x y])]
    (some (fn [pos]
            (when (= \S (char-at pos grid))
              pos)) all-coords)))

(comment
  (find-S-pos (create-grid sample-input-1))
  (find-S-pos (create-grid sample-input-2))
  (find-S-pos (create-grid (slurp "resources/day_10.txt")))
  ;; => [108 25]

  ;; Now to start from \S we must explore all 4 adjacent positions to find 2 connecting
  ;; pipes, because as \S is involved into a loop, it should connect to 2 pipes which are parts of the loop.
  ;; Actually , \S could connect a max of 4 pipes, 2 included in the loop and 2 that could lead to dead ends.
  ;; However, by looking at the sample input and the puzzle input, we can see that this is NOT the case. So
  ;; we take as postulate that \S is connected to exactly 2 pipes
  ;; Let's find them...

  (let [grid            (create-grid (slurp "resources/day_10.txt")) #_(create-grid sample-input-2)
        start-pos       (find-S-pos grid)
        adjacent-pos    (adjacent-coords start-pos grid)]
    (filter (fn [[direction _coords pipe]]
              ((matching-pipes \S direction) pipe)) adjacent-pos))

  ;; This is working fine for S : it returns 2 adjacent and connected pipes.
  ;; Let's choose one of them and find the following

  (let [grid          (create-grid (slurp "resources/day_10.txt")) #_(create-grid sample-input-2)
        current-pos   [0 0]
        pipe-at-pos   (char-at current-pos grid)
        neighbors     (adjacent-coords current-pos grid)]
    (filter (fn [[direction _coords pipe]]
              (when-let [connecting-pipes (matching-pipes pipe-at-pos direction)]
                (connecting-pipes pipe))) neighbors))

  ;; In real situation, the previous step must be removed from the neighbors, because there is no way back : once
  ;; a pipe has been used it must be ingored. So it seems we must akways remember the previous step.

  (let [grid          (create-grid (slurp "resources/day_10.txt")) #_(create-grid sample-input-2)
        current-pos   [0 0]
        prev-pos      [0 1]
        pipe-at-pos   (char-at current-pos grid)
        neighbors     (remove #(= prev-pos (second %)) (adjacent-coords current-pos grid))]
    (filter (fn [[direction _coords pipe]]
              (when-let [connecting-pipes (matching-pipes pipe-at-pos direction)]
                (connecting-pipes pipe))) neighbors))

  ;; turn it into a function
  )


(defn find-possible-next-steps [grid current-pos prev-pos]
  (let [pipe-at-pos   (char-at current-pos grid)
        neighbors     (if prev-pos
                        (remove #(= prev-pos (second %)) (adjacent-coords current-pos grid))
                          ;; Dealing with S : no previous step
                        (adjacent-coords current-pos grid))]
    (filter (fn [[direction _coords pipe]]
              (when-let [connecting-pipes (matching-pipes pipe-at-pos direction)]
                (connecting-pipes pipe))) neighbors)))

(comment
  (def grid-puzzle (create-grid (slurp "resources/day_10.txt")))
  (def grid-2 (create-grid sample-input-2))
  (def grid-1 (create-grid sample-input-1))

  (adjacent-coords [1 1] grid-1)
  (find-possible-next-steps grid-2 [1 1] nil)
  (find-possible-next-steps  grid-puzzle  [0 0] [0 1])
  (find-possible-next-steps  grid-puzzle  [10 10] nil)
  (find-possible-next-steps  grid-puzzle  [108 25] nil)

  ;; Ok, good. 
  ;; Starting from S we will have two path, each one connected to a different direction. 
  ;; We can follow these 2 paths and stop when they reach the same coordinates (the loop is closed)
  ;; At each ne step, increment a counter and return its values when stop condition is true

  ;;
  )

;; To find the next step, we must know where we come from: this is why we keep the previous step
;; and there is no need to keep all steps
(defn walk-next-step [grid [[_ current-coord :as current-step] [_ prev-coord]]]
  (vector (->> (find-possible-next-steps grid current-coord prev-coord) ;; 
               first)
          current-step))


(defn create-state [input]
  (let [grid            (create-grid input)
        start-pos       (find-S-pos grid)
        [step-1 step-2] (find-possible-next-steps grid start-pos nil)]
    {:grid            grid
     :step-count      1
     :path            [[step-1 nil] [step-2 nil]]
     :pipe-walker-fn  (partial walk-next-step grid)}))

(defn walk-the-pipes [state]
  (let [[[[_ cur-coord-1] _] [[_ cur-coord-2] _]] (:path state)]
    (if (=  cur-coord-1  cur-coord-2)
      state
      (recur (-> state
                 (update :step-count inc)
                 (update :path       (partial mapv (:pipe-walker-fn state))))))))

(defn solution-1 [input]
  (:step-count (walk-the-pipes (create-state input))))

(comment
  ;; Like usual let's try with sample input
  (solution-1 sample-input-1)
  ;; => 4 good

  (solution-1 sample-input-2)
  ;; => 8 .. still good

  ;; ..and now  
  (solution-1 (slurp "resources/day_10.txt"))
  ;; => 6701 ⭐ yesss !!

  ;;
  )

;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; see day-10-part-2


