(ns day-6
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]))


(def test-data "3,4,3,1,2")

;; part 1 ======================

(defn read-puzzle-6 [s]
  (->> (str/split s #",")
       (map (fn [s] (Integer/parseInt s)))))

(comment
  (read-puzzle-6 (slurp "./resources/puzzle_6.txt"))
  (read-puzzle-6 test-data))

(defn dec-counter
  "update fish counter value"
  [n]
  (if (zero? n) 6 (dec n)))

(defn spawn-fishes
  "Given a seq of coutner, Creates one new fish for each counter 
   at zero and returns a seq of spawned fished"
  [xs]
  (repeat (count (filter zero? xs)) 8))

(defn update-population [parent]
  (concat (map dec-counter parent) (spawn-fishes parent)))

(comment
  (update-population [3 4 3 1 2])
  (update-population '(2 3 2 0 1))
  (update-population '(1 2 1 6 0 8))
  (update-population '(0 1 0 5 6 7 8))

  (->> (take (inc 80) (iterate update-population [3 4 3 1 2]))
       ((comp count last))))

(defn solve-part-1 [s]
  (->> (read-puzzle-6 s)
       (iterate update-population)
       (take (inc 80))
       last
       count))

(comment
  (time (solve-part-1 test-data))
;; "Elapsed time: 38.4766 msecs"
;; 5934

  (time (solve-part-1 (slurp "./resources/puzzle_6.txt")))
;; "Elapsed time: 1457.0444 msecs"
;; 390923
  )

;; part 2 ================================

;; main issue here is the amount of days : 256
;; without any optimization, the test-data takes forever to
;; be processed

(defn update-population-2 [parent]
  (prn (count parent))
  (concat (doall (map dec-counter parent)) (spawn-fishes parent)))

(defn solve-part-2 [s day-count]
  (->> (read-puzzle-6 s)
       (iterate update-population-2)
       (take (inc day-count))
       last
       count))

(comment
  (time (solve-part-2 test-data 80))
;; "Elapsed time: 38.4766 msecs"
;; 5934

  (time (solve-part-2 (slurp "./resources/puzzle_6.txt") 80))
;; 80  : 1457.0444 msecs
;; 90  : 3892.75 msecs
;; 100 : 12237.0719 msecs
;; process time increases exponentially !! not good

;; let's try with the reducers library
  )

(defn reduce-counter [v]
  (->> v
       (r/reduce (fn [result counter]
                   (if (zero? counter)
                     (conj result 6 8)
                     (conj result (dec counter)))) [])
       (r/foldcat)
       (into [])))

(comment
  (reduce-counter [3,4,3,1,2])
  (reduce-counter [2 3 2 0 1])
  (reduce-counter [1 2 1 6 8 0])
  (reduce-counter [0 1 0 5 7 6 8]))

(defn solve-part-2-with-reducers [counters day]
  (if (zero? day)
    (count counters)
    (recur (reduce-counter counters)
           (dec day))))

(comment
  (time (solve-part-2-with-reducers (read-puzzle-6 test-data) 80))
;; "Elapsed time: 38.3293 msecs" (part1 : 38.4766)
;; 5934


  (time (solve-part-2-with-reducers (read-puzzle-6 (slurp "./resources/puzzle_6.txt")) 120))
;; 80  : 613.0696   msecs    (part 1 : 1457.0444)
;; 90  : 1513.1788  msecs    (part 1 : 3892.75)
;; 100 : 2732.7078  msecs    (part 1 : 12237.0719)
;; 110 : 6830.0886  msecs    (part 1 : ...)
;; 110 : 15129.0405 msecs    (part 1 : ...)
  )

;; ok that's not it. 
;; We are still increasing exponentially (but slower) the computation time
;; Implementing parallelism in computation may not be the good way to go. 
;; let's try to change the problem

;; why not count the counters by value ? counters values range is between 0 and 8 (both inclusive)
;; An array with 9 integers can be used to store the counter states. 
;; Index n contains the  amount of counters having the value n
;;
;; as we first spawn fished and THEN decrement counter by 'rest', the spawn will add a 1 to index 9
;; (which will be turned into a 8 by decrement), and add to value at index 7 (which also be moved to 6
;; by decrement)

;; day 1 =============================
;;  0 1 2 3 4 5 6 7 8 9
;; --------------------
;; [0 1 1 2 1 0 0 0 0 0] => 3,4,3,1,2
;; --- no spawn 
;; --- dec
;; [1 1 2 1 0 0 0 0 0 0] => 2,3,2,0,1

;; day2 ===============================
;; [1 1 2 1 0 0 0 0 0]
;;  ^ spawn x1    |   |
;;               +1  +1
;; [1 1 2 1 0 0 0 1 0 1]
;; --- dec
;; [1 2 1 0 0 0 1 0 1 0] => 1,2,1,6,0,8

;; day3 ===============================
;;  0 1 2 3 4 5 6 7 8 9
;; --------------------
;; [1 2 1 0 0 0 1 0 1 0] 
;;  ^ spawn x1    |   |
;;               +1  +1
;; [1 2 1 0 0 0 1 1 1 1] 
;; --- dec
;; [2 1 0 0 0 1 1 1 1 0]  =>  0,1,0,5,6,7,8

;; day4 ===============================
;;  0 1 2 3 4 5 6 7 8 9
;; --------------------
;; [2 1 0 0 0 1 1 1 1 0] 
;;  ^ spawn x2    |   |
;;               +2  +2 
;; [2 1 0 0 0 1 1 3 1 2] 
;; --- dec
;; [1 0 0 0 1 1 3 1 2 0] =>  6,0,6,4,5,6,7,8,8
;; etc ...

;; only works on vector because they are mappable
(defn spawn [[n & _ :as all]]
  (if (pos? n)
    (-> all
        (update 7 (partial + n))
        (assoc 9 n))
    all))

(spawn [1 1 2 3 4 5 6 7 8])
(spawn [0 1 1 2 1 0 0 0 0 0])

(defn solve-part-2-algo-2 [counters day]
  (if (zero? day)
    (apply + counters)
    (recur (into []                         ;; must keep a vector
                 (conj (->> counters
                            spawn
                            rest
                            (into [])) 0))
           (dec day))))

(frequencies (read-puzzle-6 test-data))

(defn create-initial-counters [s]
  (reduce (fn [result [counter-val q]]
            (assoc result counter-val q))
          (into [] (repeat 10 0))
          (frequencies (read-puzzle-6 s))))

(comment
  (create-initial-counters test-data)
  (create-initial-counters (slurp "./resources/puzzle_6.txt")))

(comment
  (time (solve-part-2-algo-2 [0 1 1 2 1 0 0 0 0 0]  80))
  (time (solve-part-2-algo-2 (create-initial-counters test-data) 80))

  (time (solve-part-2-algo-2 (create-initial-counters (slurp "./resources/puzzle_6.txt")) 256))
  ;; "Elapsed time: 1.9291 msecs"
  ;; 1749945484935

  ;; yes !!

  )

