(ns day-4
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2024/day/4

;; word search
;; find: 'XMAS'
;; find all of them
;; allows words to be horizontal, vertical, diagonal, written backwards, or even overlapping other words

;; first (naive) algo :

;; browse every position in the grid where letter 'X' is located
;;   search for 'MAS' in all 8 directions and for 3 more positions

;; other algo :
;; browse grid in 8 directions searching for XMAS
;;  - lines 
;;         - left -> right
;;         - right -> left
;;  - cols 
;;         - top -> bottom
;;         - bottom -> top
;;  - diag top-left - bottom right
;;         - down
;;         - up
;; - diag top-right - bottom left
;;         - down
;;         - up

;; let's try to go with this second algo 👍

(def sample-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defn parse-input
  "Read string s and returns a grid of char"
  [s]
  (->> (s/split-lines s)
       (mapv vec)))

(defn char-at [grid [x y]]
  (get-in grid [y x]))

(defn size
  "Returns [col-count line-count] for the given grid"
  [grid]
  [(count (first grid)) (count grid)])

(defn in-grid?
  "Return TRUE if pos [x y] is inside the given `grid`"
  [grid [x y]]
  (let [[col-count line-count] (size grid)]
    (and (< -1 x col-count)
         (< -1 y line-count))))


(defn create-segment
  "Given a grid and a seq of [x y] pos, return a seq of chars at pos"
  [grid pos-xs]
  (apply str (mapv #(char-at grid %) pos-xs)))


(defn pos-segment
  "create segment starting at `[x y]` and for the given `dx` `dy` variations"
  [grid [x y] dx dy]
  (->> (map (fn [delta]
              [(+ x (* dx delta))  (+ y (* dy delta))])  (range))
       (take-while #(in-grid? grid %))))

(defn count-xmas [s]
  (+ (count (re-seq #"XMAS" s))
     (count (re-seq #"SAMX" s))))

(defn solution-1 []
  (let [grid                   (parse-input (slurp "resources/day_4.txt"))
        [col-count line-count] (size grid)
        max-y                  (dec line-count)

        dy1 (mapv (fn [y]
                    (pos-segment grid [0 y] +1 +1)) (range 0 line-count))
        dx1 (mapv (fn [x]
                    (pos-segment grid [x 0] +1 +1)) (range 0 col-count))
        dy2 (mapv (fn [y]
                    (pos-segment grid [0 y] +1 -1)) (range 0 line-count))
        dx2 (mapv (fn [x]
                    (pos-segment grid [x max-y] +1 -1)) (range 0 col-count))
        hor (mapv (fn [y]
                    (pos-segment grid [0 y] +1 0)) (range 0 line-count))
        ver (mapv (fn [x]
                    (pos-segment grid [x 0] 0 +1)) (range 0 col-count))]

    (->> (concat dy1 dx1 dy2 dx2 hor ver)
         (filter #(< 2 (count %)))
         (map #(create-segment grid %))
         set
         (map count-xmas)
         (apply +))))

(solution-1)
;; => 2358 ⭐
