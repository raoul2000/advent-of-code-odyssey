(ns day-4
  (:require [clojure.string :as s]
            [clojure.set :as set]))


;; https://adventofcode.com/2023/day/4


;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have several 'card', each now holding 2 list of numnber:
;; - a list of 'winning' numbers
;; - a list of 'played' numbers

;; For each card we have to find how many played numbers are also winning number
;; If we found n numbers, mark this card with value 2^(n-1)
;;
;; Then sum up all cards values found to get the solution of part 1.

;; As always our first job will be to turn puzzle input (provided as string)
;; into a data structure adapted to the solution we want to implement.

(def sample-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

;; Clojure provides the clojure.set/intersection function that could be a good choice

(comment
  (set/intersection #{41 48 83 86 17} #{83 86  6 31 17  9 48 53})
  (set/intersection  #{83 86  6 31 17  9 48 53} #{41 48 83 86 17})
  ;;
  )

;; However, it must be used with sets so we will assume that both 'winning' and 'played'
;; list of number, don't contain duplicate (which makes sense in the context of this puzzle story)

;; Ok, so first, parsing the data
;; We may not need regular expressions (high cost) but only good string split

(comment
  (def s1 "Card  17: 19 25 13 51 36 71 56 65 24 50 | 13 82 73 37 83 78 48 88 87 59 97 75 18 53 44 17 84 34 79 95 69 66 76 28 57")

  (s/split s1 #" ")
  (drop 3 (s/split s1 #" "))

  (let [parts (partition-by #{"|"} (drop 3 (s/split s1 #" ")))]
    (vector (set (map #(Integer/parseInt %) (first parts)))
            (set (map #(Integer/parseInt %) (last parts)))))

  ;; let's create a nice function for this
  ;;
  )

(defn create-card [line]
  (let [parts (partition-by #{"|"} (drop 3 (s/split line #" ")))]
    (vector (set (map #(Integer/parseInt %) (first parts)))
            (set (map #(Integer/parseInt %) (last parts))))))

(comment
  (->> (s/split-lines sample-input)
       #_(map create-card))
  (create-card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
  ;; 😡 what ?! it throws an exception with the first line from sample input ! how come ?

  (s/split "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" #" ")
  ;; => ["Card" "1:" "41" "48" "83" "86" "17" "|" "83" "86" "" "6" "31" "17" "" "9" "48" "53"]
  ;; oh yes, ... see this one ?                             🔺
  ;; It for sure can't be parsed into an int, that explains the exception
  ;; Let's fix this by changing a little bit the split regex

  (s/split "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" #" +")
  ;; => ["Card" "1:" "41" "48" "83" "86" "17" "|" "83" "86" "6" "31" "17" "9" "48" "53"]

  ;; We must also change the drop number from 3 to 2
  ;; All clean.

  ;;
  )

(defn create-card-1 [line]
  (let [[winnings _ played] (partition-by #{"|"} (drop 2 (s/split line #" +")))]
    (vector (set (map #(Integer/parseInt %) winnings))
            (set (map #(Integer/parseInt %) played)))))

(comment
  (->> (s/split-lines sample-input)
       (map create-card-1))
  ;; muuuch better 👏
  ;;

  ;; Back to our intersection function. Time to try it :
  (def winners (->> (s/split-lines sample-input)
                    (map create-card-1)
                    (map (fn [[winnings played]]
                           (set/intersection winnings played)))))

  ;; We get a seq of winning numbers for each card
  ;; Remove cards with no winning numbers

  (filter seq winners)

  ;; Now we want to compute 2^(n-1) where n is the count of winning numbers

  (Math/pow 2 0)
  (Math/pow 2 1)
  (Math/pow 2 2)
  (Math/pow 2 3)
  (int (Math/pow 2 2))

  (->> winners
       (filter seq)
       (map count)
       (map #(int (Math/pow 2 (dec %))))
       (reduce +))
  ;; => 13 .. that is the correct answer for sample input

  ;; Clean up and create function for solution 1
  )

(defn solution-1 [lines]
  (->> (s/split-lines lines)
       (map create-card-1)
       (map (fn [[winnings played]]
              (set/intersection winnings played)))
       (filter seq)
       (map count)
       (map #(int (Math/pow 2 (dec %))))
       (reduce +)))

(comment
  ;; test on sample input
  (solution-1  sample-input)
  ;; still good. 

  (solution-1 (slurp "resources/day_4.txt"))
  ;; => 535235 .. and one more ⭐

  ;;
  )

;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This time, processing the card in sequence, 
;; for each card, create copies of the n following card, where n is th number of points hold by this
;; card, following part 1.

(comment

  ;; To Find the value for each card, we apply the same processing as in part one
  ;; except that we don't compute final solution.  

  (def card-value (->> (s/split-lines sample-input)
                       (map create-card-1)
                       (map (fn [[winnings played]]
                              (set/intersection winnings played)))
                       #_(filter seq)
                       (mapv count)))

  ;; for sample input we get [4 2 2 1]
  ;; card 0 holds 4 points,
  ;; card 1 holds 2 points 
  ;; etc ...

  ;; Note that actual card numbers starts with 1 in the puzzle input data
  ;; but to simplify we'll translate to zero based index. 

  ;; To get value of card 0 (card n°1 in the input data)
  (get card-value 0)
  ;; To get value of card 1 (card n°2 in the input data)
  (get card-value 1)

  ;; Now we must store how many card are there of each number.
  ;; With sample data we have at the begining : 
  ;; [1 1 1 1 1 1] : one card of each

  ;; Processing card 0 : it has a value of 4 so all 4 card following 0 must be duplicated.
  ;; This gives :
  ;; [1 2 2 2 2 1]
  ;;  ^

  ;; Processing card 1 with value of 2: the 2 card after card 1 must be duplicated but as we have
  ;; 2 cards 1, 2 following card must be duplicated onece more
  ;; - add 2 to the 2 following numbers :
  ;; [1 2 4 4 2 1]  
  ;;    ^

  ;; Processing card 2 with value of 2: the 2 card after card 2 must be duplicated but as we have
  ;; 4 cards 2, 2 following card must be duplicated twice more
  ;; - add 4 to the 2 following numbers:
  ;; [1 2 4 8 6 1]  
  ;;      ^

  ;; Processing card 3 with value of 1: the 1 card after card 3 must be duplicated but as we have
  ;; 8 cards 3, the following card must be duplicated in total 8
  ;; - add 8 to the 1 following number:
  ;; [1 2 4 8 14 1]
  ;;        ^

  ;; Processing card 4 with value of 0: no change
  ;; This gives :
  ;; [1 2 4 8 14 1]
  ;;          ^

  ;; Processing card 5 with value of 0: no change
  ;; This gives :
  ;; [1 2 4 8 14 1]
  ;;             ^

  ;; In the end we have [1 2 4 8 14 1]. 
  ;; Sum them up to get the part 2 solution (30 for sample input)


  ;; Naming things :
  ;; - idx      : zero based card number
  ;; - cnt      : card count
  ;; - card-val : card value

  ;; initially, each card only appears once. It muse be a vector
  ;; because we want to access it via index.
  ;; Working on the sample input, we have 6 cards

  (def card-count (vec (repeat 6 1)))

  ;; ... and we want to process all cards
  (def card-indexes (range 0 (count card-count)))

  (reduce (fn [acc idx]
            (let [cnt      (get acc idx)
                  card-val (get card-value idx)]
              (tap> [cnt card-val])
              (if (zero? card-val)
                acc
                acc ;; do something clever here
                )))card-count  card-indexes)


  ;; Maybe it's time to see how we can work on the card-count array to update a
  ;; slice of it.

  (defn apply-to-slice
    "Apply *f* to *n* items in *v* after position *start*"
    [start n f v]
    (vec (map-indexed (fn [idx val-idx]
                        (cond-> val-idx
                          (> (+ start (inc n)) idx start) f))  v)))

  (apply-to-slice 0 2 inc [1 1 1 1 1])
  (apply-to-slice 1 2 inc [1 1 1 1 1])
  (apply-to-slice 2 2 inc [1 1 1 1 1])
  (apply-to-slice 3 2 inc [1 1 1 1 1])
  (apply-to-slice 4 2 inc [1 1 1 1 1])

  ;; Now we can complete our reduce expression
  (reduce (fn [acc idx]
            (let [cnt      (get acc idx)
                  card-val (get card-value idx)]
              (if (zero? card-val)
                acc
                (apply-to-slice idx card-val (partial + cnt) acc)))) card-count  card-indexes)

  ;; For sample input this returns [1 2 4 8 14 1] and when added, 30 which is the
  ;; expected result.

  ;; Let's clean up and write function to solve part 2
  )

(defn compute-card-values [input]
  (->> (s/split-lines input)
       (map create-card-1)
       (map (fn [[winnings played]]
              (set/intersection winnings played)))
       (mapv count)))

(comment
  (compute-card-values sample-input)
  ;;
  )

(defn apply-to-slice
  "Apply *f* to *n* items in *v* after position *start*"
  [start n f v]
  (vec (map-indexed (fn [idx val-idx]
                      (cond-> val-idx
                        (> (+ start (inc n)) idx start) f))  v)))
(comment
  (apply-to-slice 0 2 inc [1 1 1 1 1])
  ;; 
  )

(defn solution-2 [input]
  (let [card-values      (compute-card-values input)
        card-count       (vec (repeat (count card-values) 1))
        card-indexes     (range 0 (count card-values))
        card-count-final (reduce (fn [acc idx]
                                   (let [cnt      (get acc idx)
                                         card-val (get card-values idx)]
                                     (if (zero? card-val)
                                       acc
                                       (apply-to-slice idx card-val (partial + cnt) acc)))) card-count  card-indexes)]
    (reduce + card-count-final)))

(comment
  ;; with sample input
  (solution-2 sample-input)
  ;; => 30 ok

  ;; with puzzle input ...
  (solution-2 (slurp "resources/day_4.txt"))
  ;; Yuuupiii !! one more !! ⭐ 
  )
