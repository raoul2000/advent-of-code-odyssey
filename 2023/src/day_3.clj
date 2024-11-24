(ns day-3
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/3


;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; we must explore an 'engine-schematic' which is provided as grid
;; in this grid we have numbers, characters '.' and symbols characters 
;; which are not digits nor character '.'

(defn is-symbol-char? [c]
  (not (or (Character/isDigit c)
           (= \. c))))

(defn is-digit-char? [c]
  (Character/isDigit c))

;; we must select in this grid, only number which are adjacent to a symbol 
;; and forget about the others. Then sum them up all together

;; I don't have any fancy idea to solve this one (sorry)
;; The first idea is to browse the grid line by lines, and each time we 
;; find a digit, accumulate following digits on the same line : we have a number
;  Then for each number find all the adjacent positions and search for a symbol 
;; in each one.
;; this means we need a way to navigate a 2 dimensions grid.
;;
;; let's try that

;; First we need to represent the grid

(def sample-input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

;; that's the easy part 😏

(defn input->grid
  [s]
  (->> s
       (s/split-lines)
       (mapv vec)))

(comment
  (input->grid sample-input)
  ;;
  )

(defn grid-dimensions
  "Given a grid, returns a map describing grid dimensions in terms of col and row count"
  [grid]
  {:col-count (count (first grid))
   :row-count (count grid)})

(comment
  (grid-dimensions (input->grid sample-input))
  ;;
  )

;; we will surely need grid dimensions in several places 
;; so instead of computing it each time we need it, why not create a map that contains the grid itself 
;; and its dimensions ?

(defn create-grid [s]
  (let [grid (input->grid s)]
    (merge {:matrix grid} (grid-dimensions grid))))

(comment
  (create-grid sample-input)
  ;;
  )

;; now given a x,y coord, check if it matches a point inside the grid
;; we will choose 0,0 as the top left coords

(defn in-grid [{:keys [col-count row-count]}]
  (fn [[x y]]
    (and (<= 0 x (dec col-count))
         (<= 0 y (dec row-count)))))

(comment
  (def f (in-grid (create-grid sample-input)))
  (f [1 1])
  ;;
  )

;; We will need to browse each position of the grid lines by lines, from top left to bottom right.
;; Ket's create a function to get a seq of all coords when browsing the grid this way.

(defn coords-line-by-line [{:keys [col-count row-count]}]
  (for [y (range 0  row-count)
        x (range 0  col-count)]
    [x y]))

(comment
  (coords-line-by-line {:col-count 3 :row-count 3})
  ;;
  )

;; For a given coord in the grid, we may have to check if a symbol is
;; present in one of its "adjacent positions". 
;; We need a function to create the list of adjacent positions given a position
;; and a grid

(defn adjacent-coords [[x y] grid]
  (let [pos-coll (for [dx (range -1 2)
                       dy (range -1 2)]
                   [(+ x dx) (+ y dy)])
        in-grid? (in-grid grid)]
    (->> pos-coll
         (filter in-grid?)
         (remove #{[x y]}))))


(comment
  (adjacent-coords [0 0] {:col-count 3 :row-count 3})
  ;;
  )

;; Of course we will surely need to get the char at a given position too.

(defn char-at [[x y] grid]
  (-> (:matrix grid)
      (nth y)
      (nth x)))

(comment
  (def grid-1 (create-grid sample-input))

  (char-at [0 0] grid-1)
  (char-at [1 0] grid-1)
  (char-at [0 4] grid-1)
  (char-at [9 9] grid-1)
  ;;
  )

;; And what about a function that returns all adjacent chars ?

(defn adjacent-char-coll [[x y] grid]
  (map #(char-at % grid) (adjacent-coords [x y] grid)))

(comment
  (def grid-1 (create-grid sample-input))
  (adjacent-char-coll [0 0] grid-1)
  ;;
  )

;; And now that we get there, it would be silly to not create a function
;; that returns TRUE if a pos has an adjacent symbol right ?

(defn adjacent-symbol 
  "Creates and returns a predicate function that, for a given grid,  returns true if a position 
   in this grid has adjacent symbol."
  [grid]
  (fn [[x y]]
    (some is-symbol-char? (adjacent-char-coll [x y] grid))))

(comment
  (def grid-1 (create-grid sample-input))
  (def adjacent-symbol? (adjacent-symbol grid-1))
  (adjacent-symbol? [0 0])
  (adjacent-symbol? [2 0])
  ;;
  )



;; Ok ok, it seems we have everything we need to solve the first part of this puzzle
;; But how ?
;; The ideao is the following:
;; - browse the grid char by char, line by line, from top-left to bottom right
;; - for each pos
;;     - if it is a digit
;;        | - store it in the current-number (concat)
;;        | - if no symbol has been found yet (symbol-found?)
;;        |    |  - search for a symbol in adjacent positions
;;        |    |  - if one is found
;;        |    |     |   set symbol-found to true
;;      else
;;        | - if there is a current-number and symbol-found
;;        |    |  - add this number to the list of valid parts
;;        | - reset current-number
;;        | -reset symbol-found 
;;  loop on next pos
;;
(comment

  ;; let's try to only get all numbers from the grid, without taking care of 
  ;; adjacent symbols

  (let [grid  (create-grid sample-input)
        state (merge grid {:current-number []
                           :symbol-found   false
                           :result        []})]

    (reduce (fn [state pos]
              (let [c (char-at pos state)]
                #_(tap> {:state state :pos pos})
                (if (is-digit-char? c)
                  (update state :current-number conj c)
                  (if-not (empty? (:current-number state))
                    (-> state
                        (update :result conj (Integer/parseInt (apply str (:current-number state))))
                        (assoc  :current-number []))
                    state))))
            state
            (coords-line-by-line grid)))

  ;; This is working fine (at least on the sample data) : results contains
  ;; a list of all integers found in the grid

  ;; Now let's add digits only if there is an adjacent symbol

  (let [grid             (create-grid sample-input)
        state            (merge grid {:current-number []
                                      :symbol-found   false
                                      :result         []})
        adjacent-symbol? (adjacent-symbol grid)]

    (reduce (fn [state pos]
              (let [c   (char-at pos state)]
                (tap> {:state state :pos pos :found (:symbol-found state)})
                (if (is-digit-char? c)
                  (-> state
                      (update :current-number conj c)
                      (update :symbol-found (fn [already-found]
                                              (if (not already-found)
                                                (adjacent-symbol? pos)
                                                already-found))))
                  (if-not (empty? (:current-number state))
                    (-> state ;; reset state
                        (update :result (fn [current-result]
                                          (if (:symbol-found state)
                                            (conj current-result (Integer/parseInt (apply str (:current-number state))))
                                            current-result)))
                        (assoc  :current-number [])
                        (assoc  :symbol-found false))
                    state))))
            state
            (coords-line-by-line grid)))

  ;; if we sum up all number in the result map key :result
  ;; we find the expected result for the sample input 👍
  ;; this is good !

  ;; we can create our final function and call it with the puzzle input
  ;;
  )

(defn select-valid-parts [lines]
  (let [grid             (create-grid lines)
        state            (merge grid {:current-number []
                                      :symbol-found   false
                                      :result         []})
        adjacent-symbol? (adjacent-symbol grid)]

    (reduce (fn [state pos]
              (let [c   (char-at pos state)]
                (if (is-digit-char? c)
                  (-> state
                      (update :current-number conj c)
                      (update :symbol-found   (fn [already-found]
                                                (if (not already-found)
                                                  (adjacent-symbol? pos)
                                                  already-found))))
                  (if-not (empty? (:current-number state))
                    (-> state ;; reset state
                        (update :result (fn [current-result]
                                          (if (:symbol-found state)
                                            (conj current-result (Integer/parseInt (apply str (:current-number state))))
                                            current-result)))
                        (assoc  :current-number [])
                        (assoc  :symbol-found   false))
                    state))))
            state
            (coords-line-by-line grid))))

(defn solution-1 [lines]
  (reduce + (:result (select-valid-parts lines))))

(comment

  ;; test on sample input
  (solution-1  sample-input)
  ;; still good.
  ;; and now, with the puzzle input .... (suspens)

  (solution-1 (slurp "resources/day_3.txt"))
  ;; => 535235 .. yessss !! one more ⭐

  ;; by curiosity, let's time it :
  (time (solution-1 (slurp "resources/day_3.txt")))
  ;; "Elapsed time: 119.155 msecs"

  ;;
  )

;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; now we need to recognize 'gear' which is the char '*'

(defn is-gear-char? [c]
  (= \* c))

;; Given position of a gear, we must explore all its adjacent positions
;; searching for all digits that is included in a 'part number'.
;; If we find EXACTLY 2 adjacent part number, multiply them and remember the
;; result. At the end, sum them all to get the puzzle solution.
;;
;; ok...
;;
;; So first we can simplify this by noting that any number adjacent to a
;; gear IS a part number. A gear is a special 'symbol' and any number adjacent
;; to a 'symbol' is a part number (by definition).
;;
;; ok, so ?
;; 
;; We already have from the part 1:
;; - a predicate to identify a digit : is-digit-char?
;; - a function to get all adjacent positions of a given position : adjacent-coords

;; We could use the part 1 solution and modify it so to store for each
;; part number we find, the position of all adjacent gears (and ignoring any number
;; not adjacent to a gear).
;; Then with this result, we could process gear positions and find all 
;; that appear exactly twice. Multiply those 2 adjacent part numbers and store the
;; result. At the end sum them all.


;; Let's create a function that, given a digit position, returns the positions of
;; all adjacent gear.

(defn adjacent-gears
  "Given a position returns the position of all adjacent gears or an empty seq
   if there is no adjacent gear."
  [[x y] grid]
  (->> (adjacent-coords [x y] grid)
       (filter (fn [adjacent-pos]
                 (is-gear-char? (char-at adjacent-pos grid))))))


(comment
  (adjacent-gears [2 0]  grid-1)
  (adjacent-gears [1 1]  grid-1)
  ;;
  )


(comment

  ;; let's modify the reduce operation from part 1 :

  (let [grid         (create-grid sample-input)
        state        (merge grid {:current-number []
                                  :gear-found     #{} ;; this is new
                                  :result         []})
        ->int        (fn [char-coll]
                       (Integer/parseInt (apply str char-coll)))]

    (reduce (fn [state pos]
              (let [c (char-at pos state)]
                (if (is-digit-char? c)
                  (-> state
                      (update :current-number conj c)
                      (update :gear-found into (adjacent-gears pos state)))
                  (if-not (empty? (:current-number state))
                    (-> state
                        (update :result (fn [current-result]
                                          (if-not (empty? (:gear-found state))
                                            (conj current-result (vector (->int (:current-number state))
                                                                         (:gear-found state)))
                                            current-result)))
                        (assoc :current-number [])
                        (assoc :gear-found #{}))
                    state))))
            state
            (coords-line-by-line grid)))


  ;; for the sample input the :result key contains :
  ;;
  ;; [
  ;;    [467 #{[3 1]}] 
  ;;    [35 #{[3 1]}] 
  ;;    [617 #{[3 4]}] 
  ;;    [755 #{[5 8]}] 
  ;;    [598 #{[5 8]}]
  ;; ]
  ;; Each pos is the coordinates of a gear and it is linked with the part number it is adjacent to.
  ;; We must turn this vector into a map where each key is a gear pos, and each value is the list of related
  ;; part numbers

  ;; Basically we want to turn some data structure like this :
  ;; [   
  ;;   [A [2 4 6]]
  ;;   [C [4 2]]
  ;;   [R [8 6]]
  ;; ]
  ;; ... into something like this : 
  ;; [ 
  ;;   [2 [A C]]
  ;;   [4 [A C]]
  ;;   [6 [A R]]
  ;;   [8 [R]]
  ;; ]

  ;; let's go step by step with some sample data

  (def result-1 [[467 #{[3 1]}] [35 #{[3 1]}] [617 #{[3 4]}] [755 #{[5 8]}] [598 #{[5 8]}]])

  (def grouped (group-by first (mapcat (fn [[part-number gear-pos-set]]
                                         (map #(vector % part-number) gear-pos-set)) result-1)))

  (def gear-pos-map (reduce-kv (fn [m k v]
                                 (assoc m k (map second v))) {} grouped))
  ;; result => 
  ;; {
  ;;      [3 1] (467 35), 
  ;;      [3 4] (617), 
  ;;      [5 8] (755 598)
  ;; }

  ;; Only [3 1] and [5 8] gears are adjacent to exactly 2 part number

  (def winners (remove (fn [[k v]]
                         (not= 2 (count v))) gear-pos-map))

  ;; then multiply each part number pair and sum them all

  (reduce (fn [acc [k v]]
            (+ acc (apply * v)))  0 winners)

  ;; ok we have the correct results for sample inputs.
  ;; let's clean up a bit and create functions with all we did so for.
  ;;
  )

(defn map-by-gear-pos [result]
  (->> result
       (mapcat (fn [[part-number gear-pos-set]]
                 (map #(vector % part-number) gear-pos-set)))
       (group-by first)
       (reduce-kv (fn [m k v]
                    (assoc m k (map second v))) {})))

(comment
  (def result-1 [[467 #{[3 1]}] [35 #{[3 1]}] [617 #{[3 4]}] [755 #{[5 8]}] [598 #{[5 8]}]])
  (map-by-gear-pos result-1)
  ;;
  )

(defn select-valid-parts-2 [lines]
  (let [grid         (create-grid lines)
        state        (merge grid {:current-number []
                                  :gear-found     #{}
                                  :result         []})
        ->int        (fn [char-coll]
                       (Integer/parseInt (apply str char-coll)))]

    (reduce (fn [state pos]
              (let [c (char-at pos state)]
                (if (is-digit-char? c)
                  (-> state
                      (update :current-number conj c)
                      (update :gear-found into (adjacent-gears pos state)))
                  (if-not (empty? (:current-number state))
                    (-> state
                        (update :result (fn [current-result]
                                          (if-not (empty? (:gear-found state))
                                            (conj current-result (vector (->int (:current-number state))
                                                                         (:gear-found state)))
                                            current-result)))
                        (assoc :current-number [])
                        (assoc :gear-found #{}))
                    state))))
            state
            (coords-line-by-line grid))))

(defn keep-exactly-2-adjacents [m]
  (remove (fn [[k v]]
            (not= 2 (count v))) m))

(defn solution-2 [lines]
  (->> lines
       select-valid-parts-2
       :result
       map-by-gear-pos
       keep-exactly-2-adjacents
       (reduce (fn [acc [_k v]]
                 (+ acc (apply * v)))  0)))

(comment

  ;; test on sample input
  (solution-2  sample-input)
  ;; still good.

  (solution-2 (slurp "resources/day_3.txt"))
  ;; => 79844424 ⭐ yess !! first try !!

  ;; by curiosity, let's time it :
  (time (solution-2 (slurp "resources/day_3.txt")))
  ;; "Elapsed time: 114.7777 msecs"

  ;;
  )

