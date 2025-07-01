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

(defn parse-input
  "Parse and load inpit into a map. This map contains following keys : 
   
   - `:grid` : vector of vector representing the grid
   - `:moves` : seq of move characters"
  [input]
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
  (print (s/join "\n" (grid->str grid)))
  grid)

(defn find-robot-pos
  "Find and returns the [x y] position of the robot in the given grid"
  [grid]
  (let [[col-count line-count] (grid-size grid)]
    (first (drop-while #(not= robot-char (get-at-pos grid %)) (for [x (range 0 col-count)
                                                                    y (range 0 line-count)]
                                                                [x y])))))

(def available-space-ahead? (partial some #(= % space-char)))
(defn apply-move-on-tiles
  "Given a seq of tiles, returns a new seq of tiles after robot move."
  [tiles]
  (if (available-space-ahead? tiles)
    (let [[head tail]  (split-with (partial not= space-char) tiles)]
      (concat (cons (first tail) head) (rest tail)))
    tiles))

(defn vector-positions
  "Given the robot position and a move, returns a seq of all positions from the robot
     to the grid border, excluding the border pos."
  [robot-pos grid move]
  (let [[dx dy] (cond
                  (= move-down-char move)    [identity inc]
                  (= move-up-char move)      [identity dec]
                  (= move-left-char move)    [dec identity]
                  (= move-right-char move)   [inc identity]
                  :else (throw (ex-info "invalid move" {:move move})))]
    (->> (iterate  (juxt (comp dx first) (comp dy second)) robot-pos)
         (take-while #(not=  border-char (get-at-pos grid  %))))))

(defn read-vector [grid vector-pos]
  (->> vector-pos
       (map #(get-at-pos grid %))))


;; now we have all function to access and update the grid.
;; What we need is a function like the one below : 

(defn attempt-move [grid move-char]
  (let [robot-pos     (find-robot-pos grid)
        vector-pos-xs (vector-positions robot-pos grid move-char)]
    (if (= 1 (count vector-pos-xs))
      grid
      (->> vector-pos-xs
           (read-vector grid)
           (apply-move-on-tiles)
           (map #(vector %1 %2) vector-pos-xs)
           (reduce (fn [acc [pos c]]
                     (set-at-pos acc pos c)) grid)))))

(defn apply-move
  "Given the current grid and move seq state, apply the first move and returns
   the updated grid,  and the rest of moves. Robot position may also be updated if 
   theattemps succeeded."
  [{:keys [moves grid] :as state}]
  (let [updated-grid      (attempt-move grid (first moves))
        updated-moves     (rest moves)]
    (-> state
        (assoc :grid      updated-grid)
        (assoc :moves     updated-moves))))

(defn find-all-box-pos
  "Find and returns a seq of [x y] position of box in the given grid"
  [grid]
  (let [[col-count line-count] (grid-size grid)]
    (filter #(= box-char (get-at-pos grid %)) (for [x (range 0 col-count)
                                                    y (range 0 line-count)]
                                                [x y]))))

(defn compute-final-score [grid]
  (->>
   (find-all-box-pos grid)
   (reduce (fn [acc [x y]]
             (+ acc (+ x (* 100 y)))) 0)))

(defn solution-1 [input]
  (->>
   (parse-input input)
   (iterate apply-move)
   (drop-while #(seq (:moves %)))
   first
   :grid
   (compute-final-score)))

(comment

  (solution-1 sample-input)
  ;; 10092 it taste good ..

  (solution-1 puzzle-input)
  ;; ... 1516281 ⭐
  ;;
  )
