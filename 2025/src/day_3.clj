(ns day-3
  (:require [clojure.string :as s]))


;; https://adventofcode.com/2025/day/3

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sample-input "987654321111111
811111111111119
234234234234278
818181911112111
")

(def puzzle-input (slurp "resources/day_3.txt"))

(defn parse [input]
  (->> (s/split-lines input)
       (map seq)
       (map (fn [bank]
              (map #(- (int %) 48) bank)))))

;; 1. build a seq of pairs [index num] 
;; 2. take the first one as the first digit
;;    find in rest, the first with an index value lower than the one just taken
;;    save the 2 digits found as a number
;;    go back to 2. with the rest of the pair seq
;; 3. take the max number in the seq just built
;;    

(comment

  (->> (parse sample-input)
       (map (fn [bank]
              (map-indexed (fn [k v] [k v]) bank)))
       (map (fn [indexed-bank]
              (sort-by second > indexed-bank)))
       ;;
       )

  (def seq-of-pairs '([0 9] [1 8] [2 7] [3 6] [4 5] [5 4] [6 3] [7 2] [8 1] [9 1] [10 1] [11 1] [12 1] [13 1] [14 1]))
  (def s2 '([14 9] [0 8] [1 1] [2 1] [3 1] [4 1] [5 1] [6 1] [7 1] [8 1] [9 1] [10 1] [11 1] [12 1] [13 1]))

  (loop [pairs s2
         results []]
    (if (= 1 (count pairs))
      results
      (let [cur-idx (ffirst pairs)]
        (recur (rest pairs)
               (conj results [(first pairs)
                              (->> (rest pairs)
                                   (filter #(> (first %) cur-idx))
                                   (sort-by first >)
                                   first)])))))

  (first (filter #(> (first %) 4) seq-of-pairs))










  ;;
  )