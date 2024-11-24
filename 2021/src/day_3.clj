(ns day-3
  (:require [clojure.string :as str]))

;; -- day 3 ----------------------------------

(def test-data "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")


;; part 1 =============

(defn read-puzzle-3 []
  (->> (slurp "./resources/puzzle_3.txt")
       (str/split-lines)))

(defn max-bit
  "Given a seq of char \0 or \1 returns the char
   with max occurencies"
  [xs]
  (let [[count-0 count-1] (reduce  (fn [[cnt-0 cnt-1] bit]
                                     (case bit
                                       \0 [(inc cnt-0)  cnt-1]
                                       \1 [cnt-0        (inc cnt-1)]))
                                   [0 0]
                                   xs)]
    (if (> count-0 count-1) \0 \1)))

(defn gama-epsilon
  "append the char *bit* and *not bit* to respectively *gamma* and *epsilon*
   
   Example:
   ```
   (gamma-epsilon [\"abc\" \"xyz\"] \\1) 
   => [\"abc1\" \"xyz0\"]
   ``` 
   "
  [[gamma epsilon] bit]
  [(str gamma bit)
   (str epsilon (if (= \1 bit) \0 \1))])

(->> (read-puzzle-3) ;;(str/split-lines test-data)
     (apply (partial mapv vector))
     (map max-bit)
     (reduce gama-epsilon ["" ""])
     (map #(Integer/parseInt % 2))
     (apply *))
;; => 1540244


;; part 2 =============

(defn oxygen-rating [count-0 count-1]
  (if (>= count-1 count-0) 1 0))

(defn co2-rating [count-0 count-1]
  (if (<= count-0 count-1) 0 1))

(defn filter-by-rating [report pos rating-selector]
  (let [count-1      (reduce #(+ %1 (nth  %2 pos)) 0 report)
        count-0      (- (count report) count-1)
        selected-bit (rating-selector count-0 count-1)]
    (filter #(= selected-bit (nth % pos)) report)))

(defn find-rating [rating-selector]
  (fn f
    ([report]
     (f report 0))
    ([report  pos]
     (if (= 1 (count report))
       (first report)
       (recur (filter-by-rating report pos rating-selector)
              (inc pos))))))

(defn to-digit [xs]
  (map #(Character/digit % 10) xs))

(let [report-lines (->> (read-puzzle-3)   ;;test-data
                        (map to-digit))]
  (->> [((find-rating oxygen-rating) report-lines)
        ((find-rating co2-rating)    report-lines)]
       (map #(apply str %))
       (map #(Integer/parseInt % 2))
       (apply *)))
;; => 4203981

