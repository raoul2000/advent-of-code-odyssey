(ns day-7
  (:require [clojure.string :as str]))


;; part 1 ==========================

(def test-data "16,1,2,0,4,2,7,1,2,14")

;; position 1
;; 16 - 1 = 15
;; 1  - 1 = 0
;; 2  - 1 = 1
;; 0  - 1 = 1
;; 4  - 1 = 3
;; 2  - 1 = 1
;; 7  - 1 = 6
;; 1  - 1 = 0
;; 2  - 1 = 1
;; 14 - 1 = 13
;; cost ...


(defn read-data [s]
  (->> (str/split s #",")
       (map #(Integer/parseInt %))))

(defn solve-part-1 [hpos-data]
  (loop [[target-hpos & remaining] (range 0 (apply max hpos-data))
         [_ best-cost :as result] [0 0]]
    (if (not target-hpos)
      result
      (let [cost (apply + (map #(Math/abs (- % target-hpos)) hpos-data))]
        (recur remaining
               (if (or
                    (zero? best-cost)
                    (< cost best-cost))
                 [target-hpos cost]
                 result))))))

(comment
  (time (solve-part-1 (read-data test-data)))
  ;; "Elapsed time: 2.5673 msecs"
  ;;[2 37] answer is 37

  (time (solve-part-1 (read-data (slurp "./resources/puzzle_7.txt"))))
  ;; "Elapsed time: 6315.553899 msecs"
  ;; [354 349812] answer is 349812
  )

;; part 2 ===============================

;; position 5
;; 16 - 5 = 11 = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 =  66 
;; 1  - 5 = 4  = 1 + 2 + 3 + 4 = 10
;; 2  - 5 = 3  = 1 + 2 + 3 = 6   
;; 0  - 5 = 5  = 1 + 2 + 3 + 4 + 5 = 15 
;; 4  - 1 = 3  = 1 + 2 + 3 = 6
;; etc ..
;; cost = 66 + 10 + 6 + 15 + 6 ...

;; Sum of consecutive numbers
;; =  (n*(n + 1))2
;; see https://math.stackexchange.com/questions/1100897/sum-of-consecutive-numbers

(defn compute-cost [h-pos target-pos]
  (let [step (Math/abs (- target-pos h-pos))]
    (/ (* step (inc step))
       2)))

;; same as part 1 except for computation of the cost
(defn solve-part-2 [hpos-data]
  (loop [[target-hpos & remaining] (range 0 (apply max hpos-data))
         [_ best-cost :as result] [0 0]]
    (if (not target-hpos)
      result
      (let [cost (apply + (map #(compute-cost % target-hpos) hpos-data))] ;; sum consecutive integers
        (recur remaining
               (if (or
                    (zero? best-cost)
                    (< cost best-cost))
                 [target-hpos cost]
                 result))))))

(comment
  (time (solve-part-2 (read-data test-data)))
  ;; "Elapsed time: 1.1429 msecs"
  ;; [5 168]

  (time (solve-part-2 (read-data (slurp "./resources/puzzle_7.txt"))))
  ;; "Elapsed time: 3776.7399 msecs"
  ;; [488 99763899]
  )