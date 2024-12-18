(ns day-8
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2024/day/8

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - antenna
;; - frequency
;; - antinode

(def sample-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
")

(def puzzle-input (slurp "resources/day_8.txt"))

;; working on grid again !!
;; Let's copy generic function from previous puzzles to deal with grids ..

(defn create-grid [s]
  (->> (s/split-lines s)
       (mapv vec)))

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

(defn grid->str [grid]
  (->> (map (fn [line]
              (map str line)) grid)
       (map #(apply str %))))

(defn print-grid [grid]
  (print (s/join "\n" (grid->str grid))))

(comment
  (print-grid (create-grid puzzle-input))

  ;;
  )

;; what we can do :
;; - for each antenna, get the list of all positions
;;       - build list of pairs describing each antenna pair (they generate 2 antinodes)
;;       - for each antenna pair
;;            - place antinodes

(defn grid-positions-xs
  "Returns a seq of all positions [x y] in the given grid."
  [grid]
  (let [[col-count line-count] (grid-size grid)]
    (for [x (range 0 col-count)
          y (range 0 line-count)]
      [x y])))

(comment
  (grid-positions-xs (create-grid sample-input))
  ;;
  )


(defn antennas-positions
  "Returns a map where : 
     
     - *key* : the antenna frequency
     - *value* : seq of antenna positions [x y]"
  [grid]
  (reduce (fn [res pos]
            (let [c (char-at grid pos)]
              (if (= \. c)
                res
                (update res c (fnil conj []) pos)))) {} (grid-positions-xs grid)))

(comment
  (antennas-positions (create-grid sample-input))
  ;;
  )

(defn create-pair-xs
  "Given sequence `xs`, returns a vector of all unique pair combination of items in `xs` "
  [xs]
  (loop [p xs
         res []]
    (if (empty? p)
      res
      (recur (rest p)
             (into res (map #(vector (first p) %) (rest p)))))))

(comment
  (create-pair-xs [1 2 3 4 5])
  ;;
  )

(defn antennas-pair-positions
  "Given a `grid` returns a map where :
     
     - *key* : antenna frequency (character)
     - *value* : seq of antenna pair positions `[[x1 y1] [x2 y2]]`"
  [grid]
  (->> (antennas-positions grid)
       (map (fn [[antenna-freq antenna-pos]]
              [antenna-freq (create-pair-xs antenna-pos)]))
       (into {})))

(comment
  (antennas-pair-positions (create-grid sample-input))
  ;;
  )

  ;; now let's see how to create antinodes from a pair of antennas in a grid

  ;; for example : pair [5 2] [4 4]  create 2 antinodes
  ;; - [6 0]
  ;; - [3 6]

  ;; - dx1 = 5 - 4 = 1
  ;; - dy1 = 2 - 4 = -2

  ;; - dx2 = 4 - 5 = -1
  ;; - dy2 = 4 - 2 = 2

  ;; [[x1 y1] [x2 y2]]
  ;; - dx = x1 - x2
  ;; - dy = y1 - y2

  ;; antinode1 = x1 + dx, y1 + dy
  ;; antinode2 = x2 - dx, y2 - dy

(defn create-antinodes
  "Given positions of antenna pair in a `grid`, returns a seq of antinodes positions located 
     inside the grid.
     
     At most, the returned seq will contain 2 pos. Any antinode located outside the grid is discarded.
     "
  [[[x1 y1] [x2 y2]] grid]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (filter #(in-grid? grid %) [[(+ x1 dx) (+ y1 dy)]
                                [(- x2 dx) (- y2 dy)]])))

(comment
  (create-antinodes [[5 2] [4 4]] (create-grid sample-input))
  (create-antinodes [[7 3] [8 1]] (create-grid sample-input))
    ;;
  )


(defn scan-grid
  "Returns a map describing, for each antenna frequency in the given `grid` : 
     
     - each antenna pair position 
     - all antinodes positions per antenna pair"
  [grid]
  (map (fn [[antenna-frequency antenna-pair-pos-xs]]
         [antenna-frequency (into [] (map (fn [antennas-pair-pos]
                                       {:antenna-pair-pos antennas-pair-pos
                                        :antinodes-pos   (create-antinodes antennas-pair-pos grid)})   antenna-pair-pos-xs))])
       (antennas-pair-positions grid)))

(comment
  (scan-grid (create-grid sample-input))
  ;;
  )

(defn solution-1 [input]
  (->> input
       create-grid
       scan-grid
       (mapcat second)
       (map :antinodes-pos)
       (reduce (fn [res antinode-pos-xs]
                 (into res antinode-pos-xs)) #{})
       count))

(comment

  (solution-1 sample-input)
  ;; => 14 ... good

  (solution-1 puzzle-input)
  ;; => 276 ⭐

  ;;
  )

