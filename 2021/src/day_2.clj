(ns day-2
  (:require [clojure.string :as str]))

;; -- day 2 ----------------------------------

;; part 1 =============

(defn read-puzzle-2 []
  (->> (slurp "./resources/puzzle_2.txt")
       (str/split-lines)
       (map (fn [s] (let [[dir q] (str/split s #" ")]
                      [dir (Integer/parseInt q)])))))

(defn update-coordinates-1 [[h-pos depth] [cmd val]]
  [(if (= "forward" cmd)  (+ h-pos val) h-pos)
   (case cmd
     "down" (+ depth val)
     "up"   (- depth val)
     depth)])

(->> (read-puzzle-2)
     (reduce update-coordinates-1 [0 0])
     (apply *))
;; => 1727835

;; part 2 =============

(defn update-coordinates-2 [[h-pos depth aim] [cmd val]]
  [(case cmd  ;; horizontal position
     "forward" (+ h-pos val)
     h-pos)
   (case cmd  ;; depth
     "forward" (+ depth (* aim val))
     depth)
   (case cmd  ;; aim
     "down" (+ aim val)
     "up"   (- aim val)
     aim)])

(->> (read-puzzle-2)
     (reduce update-coordinates-2 [0 0 0])
     butlast
     (apply *))
;; => 1544000595