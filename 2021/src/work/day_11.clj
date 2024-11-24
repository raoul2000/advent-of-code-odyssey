(ns work.day-11
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

;;      0 1 2 3 4 5 6 7 8 9
;; -------------------------
;; 0 | (5 4 8 3 1 4 3 2 2 3)
;; 1 | (2 7 4 5 8 5 4 7 1 1)
;; 2 | (5 2 6 4 5 5 6 1 7 3)
;; 3 | (6 1 4 1 3 3 6 1 4 6)
;; 4 | (6 3 5 7 3 8 5 4 7 8)
;; 5 | (4 1 6 7 5 2 4 6 4 5)
;; 6 | (2 1 7 6 8 4 1 7 2 1)
;; 7 | (6 8 8 2 8 8 1 1 3 4)
;; 8 | (4 8 4 6 8 4 8 5 5 4)
;; 9 | (5 2 8 3 7 5 1 5 2 6)

(defn parse-data [s]
  (->> (remove #{\newline} s)
       (map #(Character/digit % 10))
       (into [])))

(def levels (parse-data test-data))

(defn print-levels [xs]
  (doseq [line (partition 10 xs)]
    (prn line)))

(comment
  (print-levels levels))

(defn same-line [p1 p2]
  (= (quot p1 10)
     (quot p2 10)))

(defn up-pos [pos]
  (let [up (- pos 10)]
    (when (> up -1) up)))

(comment
  (nil? (up-pos 1))
  (nil? (up-pos 9))
  (= 0 (up-pos 10))
  (= 2 (up-pos 12))
  ;;
  )

(defn down-pos [pos]
  (let [down  (+ pos 10)]
    (when (< down 100) down)))

(comment
  (= 10 (down-pos 0))
  (= 15 (down-pos 5))
  (= 37 (down-pos 27))
  (= 99 (down-pos 89))
  (nil? (down-pos 90)))

(defn right-pos [pos]
  (let [right (inc pos)]
    (when (same-line pos right) right)))

(comment
  (= 1 (right-pos 0))
  (= 9 (right-pos 8))
  (nil? (right-pos 9))
  (= 13 (right-pos 12))
  (nil? (right-pos 29))
  (nil? (right-pos 99)))

(defn left-pos [pos]
  (let [left (dec pos)]
    (when (and (> left -1)
               (same-line pos left)) left)))

(comment
  (nil? (left-pos 0))
  (zero? (left-pos 1))
  (nil? (left-pos 10))
  (= 38 (left-pos 39))
  (= 10 (left-pos 11)))


(defn up-right-pos [pos]
  (when-let [up (up-pos pos)]
    (let [up-right (inc up)]
      (when (same-line up up-right) up-right))))

(comment
  (nil? (up-right-pos 0))
  (= 1 (up-right-pos 10))
  (= 9 (up-right-pos 18))
  (nil? (up-right-pos 19)))

(defn up-left-pos [pos]
  (when-let [up (up-pos pos)]
    (let [up-left (dec up)]
      (when (and (same-line up up-left)
                 (> up-left -1))
        up-left))))

(comment
  (nil? (up-left-pos 0))
  (nil? (up-left-pos 9))
  (nil? (up-left-pos 10))
  (zero? (up-left-pos 11))
  (= 8 (up-left-pos 19))
  ;;
  )

(defn down-right-pos [pos]
  (when-let [down (down-pos pos)]
    (let [down-right (inc down)]
      (when (same-line down down-right) down-right))))

(comment
  (= 19 (down-right-pos 8))
  (nil? (down-right-pos 9))
  (= 21 (down-right-pos 10))
  (nil? (down-right-pos 90)))

(defn down-left-pos [pos]
  (when-let [down (down-pos pos)]
    (let [down-left (dec down)]
      (when (and (same-line down down-left)
                 (> down-left -1))
        down-left))))

(comment
  (nil? (down-left-pos 0))
  (= 10 (down-left-pos 1))
  (= 18 (down-left-pos 9))
  (nil? (down-left-pos 90)))

(defn adjacent-pos [pos]
  (remove nil? [(up-pos pos)
                (up-right-pos pos)
                (right-pos pos)
                (down-right-pos pos)
                (down-pos pos)
                (down-left-pos pos)
                (left-pos pos)
                (up-left-pos pos)]))

(comment
  (adjacent-pos 1)
  (adjacent-pos 10)
  (adjacent-pos 11))

(defn increase-all-energy-level
  "increment all energy levels in *xs* and returns
   the updated levels"
  [xs]
  (mapv inc xs))

(defn propagate-flash-to-adjacent
  "increment energy level of all positions adjacent to *pos*"
  [pos xs]
  (reduce #(update %1 %2 inc) xs (adjacent-pos pos)))

(comment
  (do
    (print-levels levels)
    (println "---------------------")
    (print-levels (propagate-flash-to-adjacent 11 levels)))
  ;;
  )

(defn select-flashers
  "Returns the position in *xs* of all values equal greater than 9"
  [xs]
  (reduce-kv (fn [r k v]
               (if (> v 9)
                 (conj r k)
                 r)) [] xs))

(comment
  (select-flashers [1 2 9 4 9])
  ;;
  )

(defn increment-by-pos
  "Increment each values in position *pos-xs* and returns
   the modified seq"
  [pos-xs xs]
  (reduce #(if (< (get %1 %2) 9)
             (update %1 %2 inc)
             %1)
          xs
          pos-xs))

(comment
  (increment-by-pos  [0 2] [1 2 3 10]))

(defn read-adjacent-pos [pos-xs]
  (reduce (fn [r pos]
            (into r (adjacent-pos pos))) [] pos-xs))
(comment
  (read-adjacent-pos [0 1 2])
  ;;
  )

(defn select-candidates
  "Returns all positions in *pos* with a level greater than 9 in
   the grid *xs*"
  [xs pos]
  (filter #(> (get xs %) 9)  pos))

(comment
  (select-candidates [9 2 9 3 4 5 6] [0 1 2])
  ;;
  )

(defn propagate-flash-to-adjacent
  "increment energy level of all positions adjacent to *pos*
   and returns the modified seq"
  [pos xs]
  (reduce #(update %1 %2 inc) xs (adjacent-pos pos)))

(defn reset-flashers
  "set to 0 all positions in *pos-xs* and returns the modified grid"
  [xs pos-xs]
  (reduce #(assoc %1 %2 0) xs pos-xs))

(comment
  (reset-flashers [5 6 7 8 9] [0 2])
  ;;
  )

(defn flash
  ([xs candidates]
   (flash xs candidates #{}))
  ([xs candidates flashed]
   (println "begin -------------------")
   (print-levels xs)
   (println flashed)
   (let [elected (remove flashed candidates)]
     (if (empty? elected)
       (do
         (println "end propagation")
         [(reset-flashers xs flashed) flashed])

       (let [adjacents  (remove flashed (read-adjacent-pos elected))
             updated-xs (increment-by-pos adjacents xs)]
         (recur updated-xs
                (select-candidates updated-xs adjacents) ;; unique adjacent ?
                (into flashed elected)))))))



(comment
  (print-levels levels)
  (def lv2 (increase-all-energy-level (parse-data "6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637")))


  (select-flashers lv2)


  (let [[lv flash-set] (flash lv2 [49])]
    (print-levels lv)
    (print (format "flash count = %d" (count flash-set))))
  ;;
  )


(defn step [[xs _]]
  (let [grid    (increase-all-energy-level xs)
        flasher (select-flashers grid)]
    (flash grid flasher)))

(comment
  (let [[lv f] (->> (iterate step [levels #{}])
                    (take 3) ;; step + 1
                    (last))]
    (println "final ============")
    (print-levels lv)
    (prn f))


  (print-levels levels)

  (let [[lv f] (step [levels #{}])]
    (print-levels lv))

  (let [[lv f] (step (step [levels #{}]))]
    (print-levels lv)
    (prn f))

  (step [levels #{}])
  (step (step [levels #{}]))
  (increase-all-energy-level [5 4 8 3 1])
  (select-flashers (increase-all-energy-level [5 4 8 3 1]))
  (increase-all-energy-level levels)
  (select-flashers (increase-all-energy-level levels))



  (do
    (print-levels levels)
    (println "---------------------")
    (let [[lv flasher-pos] (take 2 (iterate step [levels #{}]))]
      (print-levels lv)
      (prn flasher-pos)))
  ;;


  ;;
  )


(defn inc-pos
  "Increment each values in position *pos-xs* and returns
   the modified seq"
  [pos-xs xs]
  (reduce #(update %1 %2 inc)
          xs
          pos-xs))

(defn reset-pos
  [pos-xs xs]
  (reduce #(assoc %1 %2 0)
          xs
          pos-xs))

(defn flash-2
 ;; ([levels]
 ;;  (flash-2 levels 0 #{}))
 ;; ([levels count-flash]
 ;;  (flash-2 levels count-flash #{}))
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


(comment
  (print-levels levels)
  (def lv2 (increase-all-energy-level (parse-data "6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637")))


  (select-flashers lv2)


  (let [[lv flash-count flashers] (flash-2 [lv2 0 #{}])]
    (print-levels lv)
    (println "flashers = ")
    (prn flashers)
    (print (format "flash count = %d" flash-count)))

  (flash-2 (flash-2 [lv2 0 #{}]))


  (last (take 11 (iterate flash-2 [lv2 0 #{}])))
  ;;
  )
(defn step-2 [[xs count-flash]]
  (flash-2 [(increase-all-energy-level xs)
            count-flash
            #{}]))

(comment
  (def lv2  (parse-data "6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637"))
  
  (step-2 (step-2 [lv2 0]))
  (last (take 10 (iterate step-2 [lv2 0])))
  ;; => 204 (ok)

  (last (take 100 (iterate step-2 [lv2 0])))
  ;; => 1656 (ok)

  (def lv3 (parse-data (slurp "./resources/puzzle_11.txt")))
  (def lv3 (parse-data "8271653836
7567626775
2315713316
6542655315
2453637333
1247264328
2325146614
2115843171
6182376282
2384738675"))
  
  (last (take 101 (iterate step-2 [lv3 0])))

  (let [[lv flash-count] (step-2 [lv2 0])]
    (print-levels lv)
    (print (format "flash count = %d" flash-count)))
  
  
  ;;
  )
