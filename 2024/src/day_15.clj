(ns day-15
  (:require [clojure.string :as s]
            [clojure.zip :as z]))

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


;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grid expands
;; - double width (col count)
;; - same height (line count)
;;
;; example : 
;; - before
;; ##########
;; #..O..O.O#
;; #......O.#
;; #.OO..O.O#
;; #..O@..O.#
;; #O#..O...#
;; #O..O..O.#
;; #.OO.O.OO#
;; #....O...#
;; ##########
;;
;; - after
;; ####################
;; ##....[]....[]..[]##
;; ##............[]..##
;; ##..[][]....[]..[]##
;; ##....[]@.....[]..##
;; ##[]##....[]......##
;; ##[]....[]....[]..##
;; ##..[][]..[]..[][]##
;; ##........[]......##
;; ####################
;; 
;;

(defn expand-line [line]
  (->> line
       (map (fn [tile] (case tile
                         \# [\# \#]
                         \. [\. \.]
                         \O [\[ \]]
                         \@ [\@ \.])))
       flatten
       vec))

(defn expand-grid [grid]
  (mapv expand-line grid))

(comment
  (print-grid (:grid (parse-input sample-input)))
  (print-grid (expand-grid (:grid (parse-input sample-input))))
  ;;
  )

;; moving
;;
;; Logic to handle moves, differs from part 1.
;; We can distinguish 2 patterns : 
;; - moving horizontal (left or right) - a priori easy
;; - moving vertical (up or down) - muuuch more tricky
;;
;; Moving horizontal
;; - to test if the move is possible, just scan the line
;; - changes ONLY the line where the robot is located
;; - boxes are made of unbreakable character pairs : "[]"
;; Moving vertical
;; - to scan if the move is possible .... build graph and check all leaves ?
;; - affect several cols (because box is 2 tiles width)

;; Unlike part 1, we are going to use regexp for the easy one (the horizontal moves)

(defn update-line-on-horizontal-move
  "Apply horizontal move `move-char` to the given grid line and returns it modified if the move is possible.
   The given `line` is a vec of chars."
  [line move-char]
  (let [order-fn (if (= move-char move-right-char) identity (comp vec reverse))
        line-str (apply str (order-fn line))]
    (if-let [[_ before-robot after-robot after-space] (re-matches #"(.*)@([\[\]]*)\.(.+)" line-str)]
      (->> (str before-robot ".@" after-robot after-space)
           vec
           order-fn)
      line)))

(defn move-horizontal
  "Apply the horizontal move `move-char` to the given `grid` and returns
   the new grid. When the move is not possible, returns the same grid."
  [grid move-char]
  (let [[_robot-pos-x robot-pos-y]  (find-robot-pos grid)]
    (update grid robot-pos-y update-line-on-horizontal-move move-char)))

;; Now the vertical move is more tricky

;; - before
;; ####################
;; ##....[]....[]..[]##
;; ##..[]..[]....[]..##
;; ##...[][]...[]..[]##
;; ##....[]......[]..##
;; ##[]##.@..[]......##
;; ####################
;; - after
;; ####################
;; ##..[][][]..[]..[]##
;; ##...[][].....[]..##
;; ##....[]....[]..[]##
;; ##.....@......[]..##
;; ##[]##....[]......##
;; ####################

;; We see a tree structure and so, a zipper could be the way to go

(comment
  (def grid (expand-grid (:grid (parse-input sample-input))))
  (def robot-pos (find-robot-pos grid))
  (def grid1 (-> grid
                 (set-at-pos robot-pos \.)
                 (set-at-pos [7 5] \@)))

  (defn create-fn-branch? [grid dy]
    (fn [node]
      (tap> {:create-fn-branch node})
      (let [[x y]       node
            next-pos    [x (dy y)]
            at-next-pos (get-at-pos grid next-pos)]
        (or (= \[ at-next-pos)
            (= \] at-next-pos)))))

  ((create-fn-branch? grid1 dec) [7 5])

  (defn create-fn-children [grid dy]
    (fn [branch-node]
      (tap> {:create-fn-children branch-node})
      (let [[x y]            branch-node
            at-pos           (get-at-pos grid [x y])
            [next-x next-y]  [x (dy y)]
            at-next-pos      (get-at-pos grid [next-x next-y])]
        (cond
          (and (= at-pos \[)
               (= at-next-pos \]))  [[next-x next-y] [(dec next-x) next-y]]
          (and (= at-pos \])
               (= at-next-pos \[))  [[next-x next-y] [(inc next-x) next-y]]
          (and (= at-pos \@)
               (= at-next-pos \[))  [[next-x next-y] [(inc next-x) next-y]]
          (and (= at-pos \@)
               (= at-next-pos \]))  [[next-x next-y] [(dec next-x) next-y]]
          :else [[next-x next-y]]))))

  ((create-fn-children grid1 dec) [7 5])

  (def branch?   (create-fn-branch? grid1 dec))
  (def children  (create-fn-children grid1 dec))
  (def make-node (fn [_ c]
                   (tap> {:make-node [_ c]})
                   c))

  (def  zp (z/zipper branch? children make-node [7 5]))

  ;; manual navigation ...
  (-> zp
      z/down
      z/down
      z/node)

  ;; but automatic navigation is better 
  ;; see https://grishaev.me/en/clojure-zippers/

  (defn iter-zip [zipper]
    (->> zipper
         (iterate z/next)
         (take-while (complement z/end?))))

  ;; returns all nodes
  (->> zp
       iter-zip
       (map z/node))

  ;; returns only leaves (remove branches)
  (->> zp
       iter-zip
       (remove z/branch?)
       (map z/node))

  ;; seems good. Let's put that in the code
  ;; and do some more tests to ensure it is working as expected.

  ;;
  )

(defn create-fn-branch? [grid dy]
  (fn [node]
    (let [[x y]       node
          next-pos    [x (dy y)]
          at-next-pos (get-at-pos grid next-pos)]
      (or (= \[ at-next-pos)
          (= \] at-next-pos)))))

(defn create-fn-children [grid dy]
  (fn [branch-node]
    (let [[x y]            branch-node
          at-pos           (get-at-pos grid [x y])
          [next-x next-y]  [x (dy y)]
          at-next-pos      (get-at-pos grid [next-x next-y])]
      (cond
        (and (= at-pos \[)
             (= at-next-pos \]))  [[next-x next-y] [(dec next-x) next-y]]
        (and (= at-pos \])
             (= at-next-pos \[))  [[next-x next-y] [(inc next-x) next-y]]
        (and (= at-pos \@)
             (= at-next-pos \[))  [[next-x next-y] [(inc next-x) next-y]]
        (and (= at-pos \@)
             (= at-next-pos \]))  [[next-x next-y] [(dec next-x) next-y]]
        :else [[next-x next-y]]))))

(defn create-vertical-translate-fn [vertical-move-char]
  (if (= vertical-move-char move-up-char) dec inc))

(defn iter-zip [zipper]
  (->> zipper
       (iterate z/next)
       (take-while (complement z/end?))))

(defn create-grid-zipper [grid vertical-move-char root]
  (let [dy-fn     (create-vertical-translate-fn vertical-move-char)
        branch?   (create-fn-branch?  grid dy-fn)
        children  (create-fn-children grid dy-fn)
        make-node (fn [_ c] c)]
    (z/zipper branch? children make-node root)))

(defn get-connected-tiles [grid-zipper]
  (->> grid-zipper
       iter-zip
       (map z/node)))

(defn get-leaves-tiles [grid-zipper]
  (->> grid-zipper
       iter-zip
       (remove z/branch?)
       (map z/node)))

;; So now, for a given vertical move, we can get the list of all tiles involved (so called 'connected tiles') 
;; and among them, the ones that are at the edge.

;; To find out if a vetical move is possible, all edges pos must be before a space

(defn vertical-move-possible? [grid vertical-move-char edge-boxes]
  (let [dy-fn (create-vertical-translate-fn vertical-move-char)]
    (->> edge-boxes
         (map (fn [[x y]]
                [x (dy-fn y)]))
         (map #(get-at-pos grid %))
         (every? #(= \. %)))))


(defn update-on-vertical-move
  "Move the tile `tile-char` at position `[x y]` vetically apply the `dy-fn` function on
   y in the given `grid` and returns the modified grid after move."
  [grid [[x y] tile-char] dy-fn]
  (-> grid
      (set-at-pos [x (dy-fn y)] tile-char)
      (set-at-pos [x y]         space-char)))

(defn move-veritcal [grid move-char]
  (let [dy-fn           (create-vertical-translate-fn move-char)
        grid-zipper     (create-grid-zipper grid move-char (find-robot-pos grid))
        connected-tiles (get-connected-tiles grid-zipper)
        edge-boxes      (get-leaves-tiles grid-zipper)]
    (if-not (vertical-move-possible? grid move-char edge-boxes)
      grid
      (->> connected-tiles
           ;; add char at pos as last (ex: [[x y] char])
           (map #(conj [%] (get-at-pos grid %)))
           ;; update grid
           (reduce (fn [acc cur]
                     (update-on-vertical-move acc cur dy-fn)) grid)))))

(comment

  (conj [[1 2]] 3)

  (conj [1 2] 3)
  (print-grid (expand-grid (:grid (parse-input sample-input))))
  (def grid (expand-grid (:grid (parse-input sample-input))))
  (move-veritcal grid move-up-char)
  ;;
  )
