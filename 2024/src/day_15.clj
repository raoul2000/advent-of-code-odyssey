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
(def border-char \#)

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

(defn find-robot-pos
  "Find and returns the [x y] position of the robot in the given grid"
  [grid]
  (let [[col-count line-count] (grid-size grid)]
    (first (drop-while #(not= robot-char (get-at-pos grid %)) (for [x (range 0 col-count)
                                                                    y (range 0 line-count)]
                                                                [x y])))))

(comment
  (find-robot-pos (:grid (parse-input sample-input)))
  ;;
  )

(defn create-initial-state
  "Create map describing the initial state. This map contains following keys : 
   
   - `:grid` : vector of vector representing the grid
   - `:moves` : seq of move characters
   - `:robot-pos` : [ x y ] vector representing position of the robot "
  [input]
  (let [partial-state (parse-input input)]
    (assoc partial-state :robot-pos (find-robot-pos (:grid partial-state)))))


;; now we have all function to access and update the grid.
;; What we need is a function like the one below : 

(defn attempt-move [grid move-char]
  grid)

(comment
  ;; given a move, and the tiles sequence in front of the robot in the move direction
  ;; Every tilke left by the robot is a empty tile '.'
  (def v1 [robot-char space-char space-char  box-char border-char])
  (def v2 [robot-char space-char space-char  box-char space-char border-char])
  (def v3 [robot-char box-char space-char space-char  box-char space-char border-char])
  (def v4 [robot-char box-char box-char space-char space-char  box-char space-char border-char])
  (def r1 [space-char space-char  robot-char box-char border-char])

  (partition-by #(= space-char %) v1)
  (partition-by #(= space-char %) v2)
  (partition-by #(= space-char %) v3)
  (partition-by #(= space-char %) v4)

  (concat (filter #(= \. %) v1)
          (remove #(= \. %) v1))

  (defn apply-move-on-tiles
    "Given a seq of tiles, returns a new seq of tiles after robot move."
    [tiles]
    (concat (filter #(= \. %) tiles)
            (remove #(= \. %) tiles)))

  (apply-move-on-tiles v1)
  (apply-move-on-tiles v2)
  (apply-move-on-tiles v3)
  (apply-move-on-tiles v4)

  ;;
  )



(comment
  ;; given a grid, and a vector as a pos + a direction, 
  ;; replace this vector with another one
  (def g (:grid (create-initial-state sample-input)))


  (def dpos [identity inc]) ;; move down

  ;; move down
  (take-while #(< (second %) 5) (iterate  (juxt (comp identity first) (comp inc second)) [1 1]))

  (def move move-down-char)

  (let [orig-pos [1 1]
        grid    (:grid (create-initial-state sample-input))
        [dx dy] (cond
                  (= move-down-char move)    [identity inc]
                  (= move-up-char move)      [identity dec]
                  (= move-left-char move)    [dec identity]
                  (= move-right-char move)   [inc identity]
                  :else (throw (ex-info "invalid move" {:move move})))]
    (take-while #(not=  border-char (get-at-pos grid  %)) (iterate  (juxt (comp dx first) (comp dy second)) orig-pos)))


  (defn vector-positions
    "Given the robot position and a move, returns a seq of all positions from the robot
     to the grid border, excluding the border pos."
    [orig-pos grid move]
    (let [[dx dy] (cond
                    (= move-down-char move)    [identity inc]
                    (= move-up-char move)      [identity dec]
                    (= move-left-char move)    [dec identity]
                    (= move-right-char move)   [inc identity]
                    :else (throw (ex-info "invalid move" {:move move})))]
      (take-while #(not=  border-char (get-at-pos grid  %)) (iterate  (juxt (comp dx first) (comp dy second)) orig-pos))))

  (vector-positions [1 1] g move-down-char)


  (get-at-pos g [1 1])
  (get-at-pos g [1 2])
  (get-at-pos g [1 3])

  ;; read vector

  (defn read-vector [grid vector-pos]
    (->> vector-pos
         (map #(get-at-pos grid %))))


  (defn attempt-move [grid move-char]
    (let [tiles-pos ()]))
  ;;
  )



(defn apply-move
  "Given the current grid and move seq state, apply the first move and returns
   the updated grid,  and the rest of moves. Robot position may also be updated if 
   theattemps succeeded."
  [{:keys [moves grid] :as state}]
  (let [updated-grid      (attempt-move grid (first moves))
        updated-robot-pos (find-robot-pos updated-grid)
        updated-moves     (rest moves)]
    (-> state
        (assoc :grid      updated-grid)
        (assoc :robot-pos updated-robot-pos)
        (assoc :moves     updated-moves))))

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

