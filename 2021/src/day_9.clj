(ns day-9
  (:require [clojure.string :as str]))

;; part 1 ==========================
;; we choose to keep a seq structure to represent the height map (and not try
;; to store it in a matrix)

(def test-data
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(def test-height-map (apply vector "21999432103987894921985678989287678967899899965678"))

(def row-num quot)
(defn get-adjacent
  "Returns a seq of values adjacent to the value at zero based  position *pos* in 
   the seq *xs* considered as a matrix with *col-count* columns. 
   
   Example:
   ```
   (get-adjacent 0 2 [:a :b :c :d])
   => (:b :d)
   (get-adjacent 4 3 [1 2 3 
                      4 5 6 
                      7 8 9])
   => (2 8 6 4)
   ```
   "
  [pos col-count xs]
  (let [pos-row-num      (row-num pos col-count)
        pos-right        (inc pos)
        right-same-line? (= pos-row-num
                            (row-num pos-right col-count))
        pos-left         (dec pos)
        left-same-line?  (= pos-row-num
                            (row-num pos-left col-count))]
    (remove nil? [(get xs (- pos col-count))  ;; up
                  (get xs (+ pos col-count))  ;; bottom
                  (when right-same-line?      ;; right
                    (get xs pos-right))
                  (when left-same-line?       ;; left
                    (get xs pos-left))])))

(comment
  (get-adjacent 0 10 test-height-map)
  (get-adjacent 1 10 test-height-map)
  (get-adjacent 1 2 "0011"))

(defn get-low-points [max-pos col-count height-vector]
  (reduce (fn [result pos]
            (let [heat     (get height-vector pos)
                  adjacent (get-adjacent pos col-count height-vector)]
              (if (< heat (apply min adjacent))
                (conj result heat)
                result)))
          []
          (range 0 max-pos)))

(defn solve-part-1 [s]
  (let [lines        (str/split-lines s)
        col-count    (count (first lines))
        row-count    (count lines)
        height-vector  (->> (remove #{\newline} s)
                            (map #(Character/digit % 10))
                            (apply vector))]
    (->> (get-low-points (* col-count row-count) col-count height-vector)
         (map inc)
         (apply +))))

(comment
  (solve-part-1 test-data)
  ;; => 15
  (solve-part-1 (slurp "./resources/puzzle_9.txt"))
  ;; => 508
  )

;; part-2 ===================================

(defn get-adjacent-points
  "Returns a list of point adjacents to the point at position *pos* in the matrix *xs*.
   The matrix is provided as a seq of int with dimensions *col-count* *row-count*.
   Each returned point is a map with 2 keys:
   - `:pos` : the point position (i.e index in seq)
   - `:height` : the point height

   If the point is located on an edge, less than 4 adjacents points are returned
   "
  [pos [col-count row-count] xs]
  (let [pos-row-num      (row-num pos col-count)
        pos-right        (inc pos)
        right-same-line? (= pos-row-num
                            (row-num pos-right col-count))
        pos-left         (dec pos)
        left-same-line?  (= pos-row-num
                            (row-num pos-left col-count))
        pos-up           (- pos col-count)
        up-in-grid?      (> pos-up -1)
        pos-down         (+ pos col-count)
        down-in-grid?    (< pos-down (* col-count row-count))]
    (remove (comp nil? :height) [(when up-in-grid?                    ;; up
                                   {:pos    pos-up
                                    :height (get xs pos-up)})
                                 (when down-in-grid?                  ;; down
                                   {:pos    pos-down
                                    :height (get xs pos-down)})
                                 (when right-same-line?               ;; right
                                   {:pos    pos-right
                                    :height (get xs pos-right)})
                                 (when left-same-line?                ;; left
                                   {:pos    pos-left
                                    :height (get xs pos-left)})])))

(comment
  (get-adjacent-points 0 [3 3] [0 1 2
                                3 4 5
                                6 7 8])

  (get-adjacent-points 5 [3 3] [0 1 2
                                3 4 5
                                6 7 8]))

(defn in-basin?
  "Returns *true* if the height of *point* is greater then *height* and different from 9.
   *point* is provided as a 2 keys map (`:pos`, `:height`)"
  [height point]
  (let [point-height (:height point)]
    (and (not= 9 point-height)
         (> point-height height))))

(comment
  (in-basin? 2 {:pos 1 :height 3})
  (in-basin? 2 {:pos 1 :height 4})
  (in-basin? 8 {:pos 1 :height 9}))

(defn get-adjacent-in-basin
  "Returns a list of positions for all points  adjacent to *pos* and included in
   the basin"
  [pos grid-size xs]
  (->> (get-adjacent-points pos grid-size xs)
       (filter (partial in-basin? (get xs pos)))
       (map :pos)))

(comment
  (get-adjacent-in-basin 2 [3 3] [0 1 2
                                  1 1 2
                                  6 9 8]))

(defn get-points-in-basin
  "Returns a set of positions for all points included in the basin originated at position *pos*. The point
   of origin is the lowest height point"
  [pos grid-size xs]
  (loop [basin #{}
         points #{pos}]
    (if (empty? points)
      (into #{} basin)
      (recur (into basin points)
             (into #{} (flatten (map #(get-adjacent-in-basin % grid-size xs) points)))))))

(comment
  (get-points-in-basin 0 [3 3] [0 1 2
                                1 1 2
                                6 9 8])

  (get-points-in-basin 1 [3 4] [7 6 2
                                8 7 9
                                1 9 9
                                6 9 8]))

(defn get-low-points-2
  "Returns a seq of positions for all low points in the *height-vector*"
  [max-pos col-count height-vector]
  (reduce (fn [result pos]
            (let [heat     (get height-vector pos)
                  adjacent (get-adjacent pos col-count height-vector)]
              (if (< heat (apply min adjacent))
                (conj result pos)
                result)))
          []
          (range 0 max-pos)))


(defn solve-part-2 [s]
  (let [lines        (str/split-lines s)
        col-count    (count (first lines))
        row-count    (count lines)
        height-vector  (->> (remove #{\newline} s)
                            (map #(Character/digit % 10))
                            (apply vector))]
    (->> (get-low-points-2 (* col-count row-count) col-count height-vector)
         (map #(get-points-in-basin % [col-count row-count] height-vector))
         (map count)
         sort
         (take-last 3)
         (apply *))))

(comment
  (solve-part-2 test-data)
  ;; => 1134

  (solve-part-2 (slurp "./resources/puzzle_9.txt"))
  ;; => 1564640 
  )

;; debug =================================
;; convert puzzle data to CSV file
(spit "./resources/puzzle_9.csv"
      (str/join \newline (->> (slurp "./resources/puzzle_9.txt")
                              (str/split-lines)
                              (map #(apply str (interleave % (repeat ";")))))))

