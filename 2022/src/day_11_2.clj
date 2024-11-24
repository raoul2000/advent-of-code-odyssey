(ns day-11-2
  (:require [clojure.string :refer [split]]))

;; https://adventofcode.com/2022/day/11

;; - part 1 -------------------------------------------


(def sample "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
")

(comment
  
  ;; monkey 0
  (* 79 19) ;; 1501
  (mod 1501 23) ;; not divisible -> goto monkey 3
  
  ;; monkey 3
  (+ 1501 3) ;; 1504
  (mod 1504 17) ;; not divisible -> goto monkey 1
  
  ;; monkey 1
  (+ 1504 6) ;; 1510
  (mod 1510 19) ;; not divisible -> goto monkey 0
  
  ;; monkey 0
  (* 1510 19) ;; 28690
  (mod 28690 23) ;; not divisible -> goto monkey 3
  
  ;; monkey 3
  (+ 28690 3) ;; 28693
  (mod 28693 17) ;; not divisible -> goto monkey 1
  
  ;; monkey 1
  (+ 28693 6) ;; 28699
  (mod 28699 19) ;; not divisible -> goto monkey 0
  
  ;; monkey 0
  (* 28699 19) ;; 545281
  (mod 545281 23) ;; not divisible -> goto monkey 3
  
  ;; monkey 3
  (+ 545281 3) ;; 545284
  (mod 545284 17) ;; not divisible -> goto monkey 1
  
  ;; monkey 1
  (+ 545284 6) ;; 545290
  (mod 545290 19)
  
  ;;
  )
(defn divisible-by [div num]
  (zero? (rem num div)))

(defn evaluate-operation
  "Given an arithmetic expression with 2 operands and a num representing the worry level,
   returns the result of applying the operation to the num.
   
   Expression must include variable 'old' that will be replaced with the 
   actual num value.
   
   Example:
   ```
   (evaluate-operation  \"old * old\" 10)
   => 100
   (evaluate-operation  \"old - 2\" 10)
   => 8
   ```
   "
  [^String s ^Integer worry-level]
  (let [[_ left operator right] (first (re-seq #"(.+) (.) (.+)" s))]
    (->> (str "(let [old " worry-level "M ] (mod (" operator " " left  " " right ") 96577 ))")
         (read-string)
         (eval))))

(comment
  (evaluate-operation  "old * old" 10)
  (evaluate-operation  "old * old" 10000000000000)
  (evaluate-operation  "old old" 10)
  ;;
  )

(defn item-destination
  "Given a *worry-level* and several monkey behavior settings, returns a vector
   where the first item is the new worry level, and the second item the the id
   of the monkey the item will be thrown to."
  [worry-level {:keys [increase-worry divisible? if-true if-false]}]
  (let [new-worry-level  (increase-worry worry-level)]
    (vector new-worry-level
            (if (divisible? new-worry-level)
              if-true
              if-false))))

(comment
  (item-destination 79 {:increase-worry (partial evaluate-operation "old * 19")
                        :divisible?     (partial divisible-by (Integer/parseInt "23" 10))
                        :if-true        "2"
                        :if-false       "3"})
  ;;
  )

(defn make-monkey
  "Given several strings extracted from the puzzle input and related to a single monkey, 
   returns a map describing behaviors and settings of this monkey"
  [[^String items
    ^String operation
    ^String divisible
    ^String if-true
    ^String if-false]]
  {:items           (mapv #(Integer/parseInt % 10) (split items #", "))
   :increase-worry  (partial evaluate-operation operation)
   :divisible?      (partial divisible-by (Integer/parseInt divisible 10))
   :inspected-items 0
   :if-true         if-true
   :if-false        if-false})

(defn input->monkeys
  "Converts the puzzle input into a seq of map, each one describing a monkey behavior"
  [s]
  (->> s
       (re-seq #"Monkey (\d+):\n.+items: (.+)\n.+new = (.+)\n.+by (\d+)\n.+monkey (\d+)\n.+monkey (\d+)")
       (reduce (fn [acc [_ monkey-id & other]]
                 (assoc acc monkey-id (make-monkey other)))
               {})))

(defn append-item [monkey item-value]
  (update monkey :items conj item-value))

(defn remove-first-item [monkey]
  (update monkey :items #(into [] (rest %))))

(defn inc-inspected-items [monkey]
  (update monkey :inspected-items inc))

(defn inspect-and-throw-items [inspecting-monkey-id monkeys]
  (loop [inspecting-monkey (get monkeys inspecting-monkey-id)
         items-to-inspect  (:items inspecting-monkey)
         monkeys            monkeys]
    (if (empty? items-to-inspect)
      monkeys
      (let [[new-worry-level receiver-monkey-id] (item-destination (first items-to-inspect) inspecting-monkey)]
        (recur inspecting-monkey
               (rest items-to-inspect)
               (-> monkeys
                   (update receiver-monkey-id   append-item new-worry-level)
                   (update inspecting-monkey-id remove-first-item)
                   (update inspecting-monkey-id inc-inspected-items)))))))

(comment
  (inspect-and-throw-items "0" (input->monkeys sample))
  ;;
  )

(defn play-round [monkeys]
  (loop [monkey-ids (keys monkeys)
         monkeys monkeys]
    (if (empty? monkey-ids)
      monkeys
      (recur (rest monkey-ids)
             (inspect-and-throw-items (first monkey-ids) monkeys)))))

(comment
  (play-round (input->monkeys sample))
  ;;
  )

(defn solution-2 [round-count]
  (->> (input->monkeys ;;(slurp "./resources/puzzle_11.txt")
        sample)
       (iterate play-round)
       (take (inc round-count))
       (last)
       (map (fn [[_ monkey]]
              (:inspected-items monkey)))
       (sort >)
       (take 2)
       (apply *)))

(comment
  (time (solution-2 20))
  ;; 448.263 msecs
  (time (solution-2 40))
  ;; 1005.1178
  (time (solution-2 80))
  ;; 2343.2202
  (time (solution-2 100))
  ;; 3218.0019
  (time (solution-2 1000))
  ;; 3218.0019
  )




