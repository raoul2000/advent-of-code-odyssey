(ns day-8
  (:require [clojure.string :refer [split-lines]]))

;; https://adventofcode.com/2022/day/8

(def sample "30373
25512
65332
33549
35390
")

;; the grid is represented as a vector of equal size vectors (a matrix)

(defn sample->grid
  "converts puzzle input into a vector of equal size vectors of int
   values representing tree heights."
  [s]
  (->> s
       (split-lines)
       (mapv #(mapv (fn [c] (Character/digit c 10)) %))))

(defn count-edge-trees
  "Given the tree heights grid, returns the number of unique
   trees on the grid edge."
  [grid]
  (+ (* 2 (count grid))
     (* 2 (- (count (first grid)) 2))))


(def grid-width (comp count first))
(def grid-height count)

(defn get-coll [x grid]
  (mapv #(get % x) grid))

(defn get-row [y grid]
  (get grid y))

(defn get-xy [grid x y]
  (get (into [] (for [idx (range 0 (grid-width grid))]
                  (get (get grid idx) x))) y))

(defn split-around-idx
  "Splits a vector into 2 sub vectors: one before and one after 
   the item at index *idx*. Note that item at *idx* is not present
   in neither returned sub vectors."
  [v idx]
  (reduce-kv (fn [[start end :as acc] k v]
               (cond
                 (< k idx) [(conj start v) end]
                 (> k idx) [start (conj end v)]
                 :else acc)) [[] []] v))

(defn inner-grid-xy
  "Given a grid, returns a seq of all x,y coordinates for all inner
   trees in the grid."
  [grid]
  (let [width  (grid-width grid)
        height (grid-height grid)]
    (for [x (range 1 (dec width))
          y (range 1 (dec height))]
      [x y])))

(defn visible?
  "Returns TRUE if the height at position x,y in the *grid* is visible
   from the outside, FALSE otherwise"
  [[x y] grid]
  (let [tree-height (get-xy grid x y)]
    (when-not (zero? tree-height)
      (let [[top bottom] (split-around-idx (get-coll x grid) y)
            [left right] (split-around-idx (get-row  y grid) x)]
        (some #(> tree-height (apply max %)) [top bottom left right])))))

(defn count-inner-visible-trees [grid]
  (loop [xy (inner-grid-xy grid)
         mx grid
         cnt 0]
    (if (empty? xy)
      cnt
      (recur (rest xy)
             mx
             (if (visible? (first xy) grid)
               (inc cnt)
               cnt)))))

(defn solution-1 []
  (let [grid (sample->grid
              ;;sample
              (slurp "./resources/puzzle_8.txt"))]
    (+ (count-edge-trees grid)
       (count-inner-visible-trees grid))))

(comment
  (solution-1)
  ;; => 1764 !!
  ;;
  )


;; part 2 -------------------------------------------------

;; To measure the viewing distance from a given tree, look up, down, left, and right from that tree; 
;; stop if you reach an edge or at the first tree that is the same height or taller than the tree 
;; under consideration. (If a tree is right on the edge, at least one of its viewing distances will 
;; be zero.)

(defn count-visible-trees [tree-height seq]
  (reduce (fn [acc n]
            (cond
              (< n tree-height)  (inc acc)
              (>= n tree-height) (reduced (inc acc))
              :else acc)) 0 seq))

(defn full-grid-xy
  "Given a grid, returns a seq of all x,y coordinates for all trees in the grid."
  [grid]
  (let [width  (grid-width grid)
        height (grid-height grid)]
    (for [x (range 0  width)
          y (range 0 height)]
      [x y])))


(defn scenic-score [grid [x y]]
  (let [tree-height  (get-xy grid x y)
        [top bottom] (split-around-idx (get-coll x grid) y)
        [left right] (split-around-idx (get-row  y grid) x)]
    (->> [(reverse top) bottom (reverse left) right]
         (map #(count-visible-trees tree-height %))
         (apply *))))

(defn solution-2 []
  (let [grid (sample->grid
              ;;sample
              (slurp "./resources/puzzle_8.txt")
              )]
    (->> (full-grid-xy grid)
         (map #(scenic-score grid %))
         (apply max))))

(comment
  (solution-2)
  ;; => 199272 !!
  )
