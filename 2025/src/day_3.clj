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

  (def s1 '([0 9] [1 8] [2 7] [3 6] [4 5] [5 4] [6 3] [7 2] [8 1] [9 1] [10 1] [11 1] [12 1] [13 1] [14 1]))
  (def s2 '([14 9] [0 8] [1 1] [2 1] [3 1] [4 1] [5 1] [6 1] [7 1] [8 1] [9 1] [10 1] [11 1] [12 1] [13 1]))
  (def s3 '([14 8] [13 7] [2 4] [5 4] [8 4] [11 4] [1 3] [4 3] [7 3] [10 3] [0 2] [3 2] [6 2] [9 2] [12 2]))
  (def s4 '([6 9] [0 8] [2 8] [4 8] [11 2] [1 1] [3 1] [5 1] [7 1] [8 1] [9 1] [10 1] [12 1] [13 1] [14 1]))


  (defn find-max-pair [cur-idx indexed-bank]
    (->> indexed-bank
         (filter #(> (first %) cur-idx))
         first))

  (defn find-max-joltage [bank]
    (->> bank
         (map (fn [[cur-idx cur-v]]
                [cur-v (find-max-pair cur-idx bank)]))
         (filter second)
         (map (fn [[digit1 [_idx digit2]]]
                (+ (* 10 digit1) digit2)))
         first))

  (find-max-joltage s1)
  (find-max-joltage s4)
  ;;
  )


(defn find-max-pair [cur-idx indexed-bank]
  (->> indexed-bank
       (filter #(> (first %) cur-idx))
       first))

(defn find-max-joltage [bank]
  (->> bank
       (map (fn [[cur-idx cur-v]]
              [cur-v (find-max-pair cur-idx bank)]))
       (filter second)
       (map (fn [[digit1 [_idx digit2]]]
              (+ (* 10 digit1) digit2)))
       first))

(defn solution-1 [s]
  (->> (parse s)
       (map (fn [bank]
              (map-indexed (fn [k v] [k v]) bank)))
       (map (fn [indexed-bank]
              (sort-by second > indexed-bank)))
       (map find-max-joltage)
       (reduce +)))

(comment
  (solution-1 sample-input)
  ;; => 357 ... good

  (solution-1 puzzle-input)
  ;; => 17535 ⭐ yes!

  ;;
  )

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 12 batteries instead of 2

;; Here is an idea for a solution : 
;;
;;
;; bank : 818181911112111 (len = 15)
;; result : ""
;;     - len bank - (12 - len result) = 3
;;     - bag = 8 1 8
;;     - max = ^

;; bank : .18181911112111 (len = 14)
;; result : "8" (len = 1)
;;     - len bank - (12 - len result) = 14 - 11 = 3
;;     - bag = 1 8 1
;;     - max =   ^

;; bank : ...181911112111 (len = 12)
;; result : "88"
;;     - len bank - (12 - len result) = 12 - 10 = 2
;;     - bag = 1 8
;;     - max =   ^

;; bank : .....1911112111 (len = 11)
;; result : "888"
;;     - len bank - (12 - len result) = 11 - 9 = 2
;;     - bag = 1 9
;;     - max =   ^

;; bank :  .......11112111 (len = 9)
;; result : "8889"
;;     - len bank - (12 - len result) = 9 - 8 = 1
;;     - bag = 1
;;     - max = ^

;; bank :  ........1112111 (len = 8)
;; result : "88891"
;;     - len bank - (12 - len result) = 8 - 7 = 1
;;     - bag = 1
;;     - max = ^

;; etc ....

(comment

  ;; we need a function to split a vec of integer at the position of the first
  ;; occurence of the max value in the vec
  
  (defn first-max-kv-reducer [acc k v]
    (if (or (empty? acc)
            (> v (second acc)))
      [k v]
      acc))

  (defn split-at-first-max
    "Split the seq of intergers in 2 after the position of the first occurence
     of (max v-xs).
     
     Example:
     ```
     (split-at-first-max [1 2 3 3 2 1 1])
     => [(1 2 3) (3 2 1 1)]

     (split-at-first-max [2 2 1 3])
     => [(2 2 1 3) ()]
     ```
     "
    [v-xs]
    (let [max-idx  (->> v-xs
                        vec
                        (reduce-kv first-max-kv-reducer [])
                        first)]
      (split-at (inc max-idx) v-xs)))

  (split-at-first-max [1 2 3 3 2 1 1])
  (split-at-first-max [2 2 1 3])
  (split-at-first-max '(2 2 1 3))

  ;; now the main loop
  (tap> "boo")
  
  (loop [bank [8 1 8 1 8 1 9 1 1 1 1 2 1 1 1]
         iteration-num 1
         result []]
    (if (zero? (count bank))
      result
      (let [bank-len      (count bank)
            result-len    (count result)
            bag-len       (- bank-len (- 12 result-len))
            bag           (take bag-len bank)
            [keep re-use] (split-at-first-max bag)]
        (tap> {:iteration-num iteration-num
               :bank bank
               :result result
               :bag    bag 
               :keep   keep
               :re-use re-use})
        (recur (-> []
                   (into re-use)
                   (into (drop bag-len bank)))
               (inc iteration-num)
               (conj result (last keep))))))

  (last ())
  (conj [1] '())
  (conj [1] nil)
  (vec (take 2 [1 2 3]))
  (-> []
      (into [3 4])
      (into [5 6]))
  (into [1 2] '(3 4))

  (take 2 [1 2 3])
  (conj [] 1)

  ;;
  )


