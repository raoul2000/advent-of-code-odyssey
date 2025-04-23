(ns day-15
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2024/day/15

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; - one robot on a grid
;; - robot attempts to move
;;    - up : ^
;;    - right : >
;;    - left : <
;;    - down : v
;; - grid contains robot '@', boxes 'O', limits '#' and free space '.'



(def sample-input "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
")

(def puzzle-input (slurp "resources/day_15.txt"))

;; on the grid
(def robot-char \@)
(def box-char \O)
(def space-char \.)
(def limit-char \#)

;; moves
(def move-up-char \^)
(def move-right-char \>)
(def move-left-char \<)
(def move-down-char \v)

;; parse inputs
;; - first part is the grid
;; - empty line
;; - second part are the moves


(defn parse-grid [lines]
  (->> lines
       (map vec)
       vec))

(defn parse-moves [lines]
  (vec (s/join lines)))

(defn parse-input [input]
  (let [[grid-str _separator moves-str] (partition-by #(= % "") (s/split-lines input))]
    {:grid  (parse-grid grid-str)
     :moves (parse-moves moves-str)}))

(comment
  (parse-input sample-input)
  (parse-input puzzle-input)
  ;;
  )

(defn grid-size
  "Returns [col-count line-count] for the given grid"
  [grid]
  [(count (first grid)) (count grid)])

(defn get-at-pos [grid [x y]]
  (get-in grid [y x]))

(defn set-at-pos [grid [x y] c]
  (update-in grid [y x] (constantly c)))

(defn grid->str [grid]
  (->> (map (fn [line]
              (map str line)) grid)
       (map #(apply str %))))

(defn print-grid [grid]
  (print (s/join "\n" (grid->str grid))))

(comment
  (def grid-1 (:grid (parse-input sample-input)))
  (grid-size grid-1)
  (print-grid grid-1)
  (->> (set-at-pos grid-1 [4 4] \Z) print-grid)
  ;;
  )

(defn find-robot-initial-pos
  "Find and returns the [x y] position of the robot in the given grid"
  [grid]
  (let [[col-count line-count] (grid-size grid)]
    (first (drop-while #(not= robot-char (get-at-pos grid %)) (for [x (range 0 col-count)
                                                                    y (range 0 line-count)]
                                                                [x y])))))

(comment
  (find-robot-initial-pos (:grid (parse-input sample-input)))
  ;;
  )

(defn create-initial-state
  "Create map describing the initial state. This map contains following keys : 
   
   - `:grid` : vector of vector representing the grid
   - `:moves` : seq of move characters
   - `:robot-pos` : [ x y ] vector representing position of the robot "
  [input]
  (let [partial-state (parse-input input)]
    (assoc partial-state :robot-pos (find-robot-initial-pos (:grid partial-state)))))


;; now we have all function to access and update the grid.
;; What we need is a function like the one below : 

(defn attempt-move [grid move-char]
  grid)

(defn apply-move
  "Given the current grid and move seq state, apply the first move and returns
   the updated grid,  and the rest of moves. Robot position may also be updated if 
   theattemps succeeded."
  [{:keys [moves] :as state}]
  (-> state
      (update :grid  attempt-move (first moves))
      (update :moves rest)))

;; ...and then call apply-move until no more moves are to process

(comment
  (def state (create-initial-state sample-input))
  (def state (create-initial-state puzzle-input))
  (count (:moves state))

  (->> state
       apply-move
       apply-move
       apply-move
       :moves
       count)

  (time (first (drop-while  #(seq (:moves %)) (iterate apply-move state))))
  ;;
  )

