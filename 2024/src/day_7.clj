(ns day-7
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))


;; https://adventofcode.com/2024/day/7

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - evaluated left-to-right
;; - operators : + *

(def sample-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
")

(def puzzle-input (slurp "resources/day_7.txt"))

(defn parse-input [s]
  (->>
   (s/split-lines s)
   (map #(s/split % #"[: ]"))
   (map #(remove (partial = "") %))
   ;; use edn/read-string instead of Integer/parseInt
   (map #(map (fn [n] (edn/read-string n)) %))))

(comment
  (parse-input sample-input)
  (parse-input puzzle-input)
  (Integer/parseInt "1455501866546")
  (edn/read-string "1455501866546")
;;
  )

(defn reduce-computation [target-result num-xs]
  (reduce (fn [res n]
            (let [new-result (->> res
                                  (map (juxt (partial + n) (partial * n)))
                                  flatten
                                  (remove #(> % target-result)))]
              (if (empty? new-result)
                (reduced false)
                new-result)))
          [(first num-xs)]
          (rest num-xs)))



(defn solution-1 [input]
  (->> input
       parse-input
       (map (fn [n-xs]
              (let [target-n (first n-xs)
                    operands (rest n-xs)
                    computations (->> (reduce-computation target-n operands)
                                      (some (partial = target-n)))]
                (when computations target-n))))
       (remove nil?)
       (apply +)))

(comment
  (solution-1 sample-input)
  ;; => 3749 (good)

  (solution-1 puzzle-input)
  ;; => 7579994664753 ⭐

  ;;
  )

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - new operators : || (concatenation)
;; We will just keep the same algo than part 1 and introduce this new operator

(defn concatenation-operator [a b]
  (edn/read-string (str a b)))

(defn reduce-computation-ex [target-result num-xs]
  (reduce (fn [res n]
            (let [new-result (->> res
                                  (map (juxt (partial + n) (partial * n) #(concatenation-operator % n)))
                                  flatten
                                  (remove #(> % target-result)))]
              (if (empty? new-result)
                (reduced false)
                new-result)))
          [(first num-xs)]
          (rest num-xs)))

(defn solution-2 [input]
  (->> input
       parse-input
       (map (fn [n-xs]
              (let [target-n (first n-xs)
                    operands (rest n-xs)
                    computations (->> (reduce-computation-ex target-n operands)
                                      (some (partial = target-n)))]
                (when computations target-n))))
       (remove nil?)
       (apply +)))

(comment
  (solution-2 sample-input)
  ;; => 11387 (good)

  (solution-2 puzzle-input)
  ;; => 438027111276610 ⭐

  ;;
  )


