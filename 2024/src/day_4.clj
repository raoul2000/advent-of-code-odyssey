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
  (->> (s/split-lines sample-input)
       (mapv vec)))

(defn char-at [grid [x y]]
  (get-in grid [x y]))

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

(comment

  (def g (parse-input sample-input))
  (char-at g [0 0])
  (char-at g [0 2])
  (char-at g [0 3])
  (char-at g [1 1])

  (size g)

  ;; loop over lines pos
  (let [[col-count line-count] (size g)]
    (for [x (range 0 col-count)
          y (range 0 line-count)]
      [x y]))


  ;; loop over cols
  (let [[col-count line-count] (size g)]
    (for [x (range 0 col-count)
          y (range 0 line-count)]
      [y 0]))

  ;; loop over diagonals top-left -> bottom-right
  ;; all segment starting points
  (let [[col-count line-count] (size g)]
    (for [y (range (dec line-count) -1 -1)]
      [0 y]))

  (let [[col-count line-count] (size g)]
    (for [x (range 1 col-count)]
      [x 0]))

  ;; all diagonal\ segment starting pos 
  (let [[col-count line-count] (size g)]
    (apply conj
           (mapv #(vector 0 %) (range 0 line-count))   ;; right vertical
           (mapv #(vector % 0) (range 1 col-count))))  ;; top horiz

  ;; all diagonal /  segment starting pos   
  (let [[col-count line-count] (size g)]
    (apply conj
           (mapv #(vector % (dec line-count)) (range 0 col-count))    ;; bottom horiz
           (mapv #(vector (dec col-count) %)  (range 0 line-count)))) ;; right vertical


  ;; given a diagonal staring pos find all pos in the segment
  (->> (map (fn [delta]
              [(+ 0 delta) (+ 0 delta)])  (range 0 20))
       (take-while #(in-grid? g %)))

  /
  (defn create-segment-backslash
    "create all pos for segment \\ starting at `[x y]`"
    [grid [x y]]
    (->> (map (fn [delta]
                [(+ x delta) (+ y delta)])  (range))
         (take-while #(in-grid? grid %))))

  (create-segment-backslash g [0 0])
  (create-segment-backslash g [0 1])
  (create-segment-backslash g [5 0])
  (create-segment-backslash g [10 0])

  (defn create-segment-slash
    "create all pos for segment / starting at `[x y]`"
    [grid [x y]]
    (->> (map (fn [delta]
                [(- x delta) (+ y delta)])  (range))
         (take-while #(in-grid? grid %))))

  (create-segment-slash g [3 0])
  (create-segment-slash g [3 0])


  (let [[col-count line-count] (size g)]
    (mapv #(vector 0 %) (range 0 line-count)))


  (defn create-segment
    "Given a grid and a seq of [x y] pos, return a seq of chars at pos"
    [grid pos-xs]
    (apply str (mapv #(char-at grid %) pos-xs)))

  (defn count-xmas [s]
    (+ (count (re-seq #"XMAS" s))
       (count (re-seq #"SAMX" s))))

  (count-xmas "..XMAS..XM..XMAS..SAMX..XMAS")
  ;;
  )

(defn segments-backslash-starting-pos
  "all diagonal \\ segment starting pos "
  [grid]
  (let [[col-count line-count] (size grid)]
    (apply conj
           (mapv #(vector 0 %) (range 0 line-count))   ;; right vertical
           (mapv #(vector % 0) (range 1 col-count))))  ;; top horiz
  )
