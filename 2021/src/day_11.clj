(ns day-11
  (:require [clojure.string :as str]))

(def test-data "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(defn parse-data [s]
  (->> (remove #{\newline} s)
       (map #(Character/digit % 10))
       (into [])))

;; functions to navigate in the sequence on levels has if it
;; was a 5x5 matrix. 

(defn same-line [p1 p2]
  (= (quot p1 10)
     (quot p2 10)))

(defn up-pos [pos]
  (let [up (- pos 10)]
    (when (> up -1) up)))

(defn down-pos [pos]
  (let [down  (+ pos 10)]
    (when (< down 100) down)))

(defn right-pos [pos]
  (let [right (inc pos)]
    (when (same-line pos right) right)))

(defn left-pos [pos]
  (let [left (dec pos)]
    (when (and (> left -1)
               (same-line pos left)) left)))

(defn up-right-pos [pos]
  (when-let [up (up-pos pos)]
    (let [up-right (inc up)]
      (when (same-line up up-right) up-right))))

(defn up-left-pos [pos]
  (when-let [up (up-pos pos)]
    (let [up-left (dec up)]
      (when (and (same-line up up-left)
                 (> up-left -1))
        up-left))))

(defn down-right-pos [pos]
  (when-let [down (down-pos pos)]
    (let [down-right (inc down)]
      (when (same-line down down-right) down-right))))

(defn down-left-pos [pos]
  (when-let [down (down-pos pos)]
    (let [down-left (dec down)]
      (when (and (same-line down down-left)
                 (> down-left -1))
        down-left))))

(defn adjacent-pos [pos]
  (remove nil? [(up-pos pos)
                (up-right-pos pos)
                (right-pos pos)
                (down-right-pos pos)
                (down-pos pos)
                (down-left-pos pos)
                (left-pos pos)
                (up-left-pos pos)]))

(defn read-adjacent-pos [pos-xs]
  (reduce (fn [r pos]
            (into r (adjacent-pos pos))) [] pos-xs))

;; helpers function 

(defn select-flashers
  "Returns the position in *xs* of all values equal greater than 9"
  [xs]
  (reduce-kv (fn [r k v]
               (if (> v 9)
                 (conj r k)
                 r)) [] xs))

(defn reset-pos
  [pos-xs xs]
  (reduce #(assoc %1 %2 0)
          xs
          pos-xs))

(defn inc-pos
  "Increment each values in position *pos-xs* and returns
   the modified seq"
  [pos-xs xs]
  (reduce #(update %1 %2 inc)
          xs
          pos-xs))

(defn increase-all-energy-level
  "increment all energy levels in *xs* and returns
   the updated levels"
  [xs]
  (mapv inc xs))

;; solving the puzzle

(defn flash
  ([[levels count-flash flashed]]
   (let [candidates (select-flashers levels)
         elected    (remove flashed candidates)]
     (if (empty? elected)
       [(reset-pos flashed levels)
        (+ count-flash (count flashed))
        flashed]
       (let [all-adjacent       (read-adjacent-pos elected)
             adjacent-no-flash  (remove flashed all-adjacent)
             levels-after-flash (->> levels
                                     (reset-pos elected)
                                     (inc-pos adjacent-no-flash))]
         (recur [levels-after-flash
                 count-flash
                 (into flashed elected)]))))))

(defn do-step [[xs count-flash]]
  (flash [(increase-all-energy-level xs)
          count-flash
          #{}]))

(defn solve-part-1 [s step-count]
  (let [levels (parse-data s)]
    (->> (iterate do-step [levels 0])
         (take (inc step-count))
         last
         second)))

(comment
  (solve-part-1 test-data 10)
  ;; => 204
  (solve-part-1 test-data 100)
  ;; => 1656

  (solve-part-1 (slurp "./resources/puzzle_11.txt") 100)
  ;; => 1562
  )

;; part 2 ======================================

(defn solve-part-2 [s]
  (->> (iterate do-step [(parse-data s) 0])
       (take-while (fn [[_ _ flashed]]
                     (not= (count flashed) 100)))
       count))

(comment
  (solve-part-2 test-data)
  ;; => 195
  (solve-part-2 (slurp "./resources/puzzle_11.txt"))
  ;; => 268
  )


