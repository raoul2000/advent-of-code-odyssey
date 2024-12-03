(ns day-3
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2024/day/3

;; corrupted memory
;; multiply some numbers
;; instructions like mul(X,Y), where X and Y are each 1-3 digit

;; extract mul(nnn,nnn)
;; let's create a regexp

(def sample-data "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(comment
  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" sample-data)
  ;;
  )


(defn solution-1 []
  (->> (slurp "resources/day_3.txt")
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map rest)
       (map (fn [[n1 n2]]
              (* (Integer/parseInt n1)
                 (Integer/parseInt n2))))
       (apply +)))

(solution-1)
;; => 185797128 ⭐

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; conditional statements

;; - The do() instruction enables future mul instructions.
;; - The don't() instruction disables future mul instructions.

(def sample-data-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")


(comment
  ;; another regexp

  (re-seq #"mul\(\d{1,3},\d{1,3}\)|don't\(\)|do\(\)" sample-data-2)
  ;; => ("mul(2,4)" "don't()" "mul(5,5)" "mul(11,8)" "do()" "mul(8,5)")

  ;; reducing
  (reduce (fn [acc token]
            (case token
              "don't()"     (assoc acc :do false)
              "do()"        (assoc acc :do true)
              (if (:do acc)
                (update acc :sum (partial + 1))
                acc))) {:do true
                        :sum 0}
          (re-seq #"mul\(\d{1,3},\d{1,3}\)|don't\(\)|do\(\)" sample-data-2))

  ;;
  )

(defn multiply [s]
  (->> (re-matches #"mul\((\d{1,3}),(\d{1,3})\)" s)
       rest
       (map #(Integer/parseInt %))
       (apply *)))

(defn solution-2 []
  (->> (slurp "resources/day_3.txt")
       (re-seq #"mul\(\d{1,3},\d{1,3}\)|don't\(\)|do\(\)")
       (reduce (fn [acc token]
                 (case token
                   "don't()"     (assoc acc :do false)
                   "do()"        (assoc acc :do true)
                   (cond-> acc
                     (:do acc) (update :sum (partial + (multiply token)))))) {:do true
                                                                              :sum 0})))

(solution-2)
;; => {:do true, :sum 89798695} ⭐


