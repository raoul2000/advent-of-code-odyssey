(ns day-2-read-again
  (:require [clojure.string :as s]))

;; welcome back !
;; Now I have read more carefully the puzzle (I hope so) let's see if we can solve it

;; same sample input
(def sample-input-1 "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")


(comment
  ;; we want to split each handulf in :
  ;; - the game id
  ;; - a seq of colored cube count

  (re-matches #"Game (\d+):(.*)"
              "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")

  (map (fn [subset]
         (map rest  (re-seq #"((\d+) (green|red|blue))" subset))) (s/split  " 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue" #";"))

  ;; working on the subset to get a list of <color count, color name> map for each subset
  (->> (s/split  " 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue" #";")  ;; split by subset
       (map (fn [subset]
              (->> (re-seq #"((\d+) (green|red|blue))" subset)                      ;; re-match on a subset
                   (map (fn [[_match-1 _match-2 c color]]
                          (vector (Integer/parseInt c) color)))                     ;; keep color count and color name
                   (reduce (fn [m [n color]]                                        ;; aggegrate to map k = color name, v = color count
                             (assoc m color n)) {})))))

  ;; result example : (
  ;;    {"blue" 1, "green" 2}
  ;;    {"green" 3, "blue" 4, "red" 1}
  ;;    {"green" 1, "blue" 1}
  ;; )
  ;;

  ;; hold on ! I'm not sure a map is required here, and probably there is no need to maintain
  ;; separation between subsets... too complex for what we need to achieve

  (->> (re-seq #"((\d+) (green|red|blue))" " 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
       (map (fn [[_fullmatch _group1 num-s color]]
              (vector (Integer/parseInt num-s) color))))

  ;; some data like this should be enough
  ;; => ([1 "blue"] [2 "green"] [3 "green"] [4 "blue"] [1 "red"] [1 "green"] [1 "blue"])
  ;; In the end we just need to check that one of these item is not compliant with bag content

  ;; We want to keep the game Id associated with this normalized handful data structure.
  ;; let's turn that into a function ..
  )

(defn normalize-handful [handful]
  (let [[_fullmatch game-id right-part] (re-matches #"Game (\d+):(.*)" handful)
        cube-sets                       (->> (re-seq #"((\d+) (green|red|blue))" right-part)
                                             (map (fn [[_fullmatch _group1 num-s color]]
                                                    (vector (Integer/parseInt num-s) color))))]
    (vector (Integer/parseInt game-id) cube-sets)))

(comment
  (normalize-handful "Game 28: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
  ;; => [28 ([1 "blue"] [2 "green"] [3 "green"] [4 "blue"] [1 "red"] [1 "green"] [1 "blue"])]

  ;; With this data structure we can check that for all cube-sets, no color count is greater than
  ;; what's in the bag.

  (def cube-sets (normalize-handful "Game 28: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"))
    ;; assuming this is the bag content
  (def bag {"red"    12
            "green"  1
            "blue"   12})

  ;; evaluate to TRUE if this is an impossible handful
  (->> (second cube-sets)
       (some (fn [[cnt color]]
               (> cnt (get bag color)))))
  ;; let's go for another function
  )

(defn impossible-handful
  "Given bag content, returns a predicate that returns TRUE if the handful cannot be obtained
  given the provided bag color distribution"
  [bag]
  (fn [[_game-id cube-set]]
    (some (fn [[color-count color-name]]
            (> color-count (get bag color-name))) cube-set)))

;; we are ready (again) to test that new solution

(defn solution-1 [handful-list bag]
  (let [impossible-handful? (impossible-handful bag)]
    (->> handful-list
         (map normalize-handful)
         (remove impossible-handful?)
         (reduce (fn [sum [game-id _cube-set]]
                   (+ sum game-id)) 0))))

(comment
  (solution-1 (s/split-lines sample-input-1) {"red"   12
                                              "green" 13
                                              "blue"  14})
  ;; 👍 sample data give correct answer 8
  ;; let's try with puzzle data

  (solution-1 (s/split-lines (slurp "resources/day_2.txt")) {"red"   12
                                                             "green" 13
                                                             "blue"  14})
  ;; => 2006  Yessss !! ⭐ (at last)

  ;;
  )


;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; we have to process each handful and for each one of them, compute a number
;; then sum all numbers to get the solution
;; Each number is the product of 3 other numbers corresponding to the max color
;; count (for each one of the three colors)

(comment
  ;; start from our normalize-handful function
  (def handful-1 (normalize-handful "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))

  ;; could we use group-by ?
  (group-by second (second handful-1))
  ;; => {"blue" [[3 "blue"] [6 "blue"]], "red" [[4 "red"] [1 "red"]], "green" [[2 "green"] [2 "green"]]}

  (map (fn [[color color-set]]
         (vector color (apply max (map first color-set))))
       (group-by second (second handful-1)))

  (->> (second handful-1)                  ;; consider only the cube color info part
       (group-by second)                   ;; group by color name
       (map (fn [[color color-set]]        ;; replace values with max first
              (vector color (apply max (map first color-set)))))
       (reduce (fn [acc [_color-name n]]   ;; multiply them all
                 (* acc n)) 1))


  ;; We should have enough to start writing down the solution
  ;;
  )

(defn compute-min-color-product [handful]
  (->> (normalize-handful handful)
       (second)
       (group-by second)
       (map (fn [[color color-set]]
              (vector color (apply max (map first color-set)))))
       (reduce (fn [acc [_color-name n]]
                 (* acc n)) 1)))

(comment
  (compute-min-color-product "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
  ;; => 48 .. this is the expected anwser
  ;;
  )

(defn solution-1-part-2 [handful-list]
  (->> (map compute-min-color-product handful-list)
      (reduce + )))

(comment
    (solution-1-part-2 (s/split-lines sample-input-1))
    ;; 👍 sample data give correct answer 2286
    ;; let's try with puzzle data
  
  (solution-1-part-2 (s/split-lines (slurp "resources/day_2.txt")))
    ;; => 84911  Yessss !! ⭐ 
  ;;
  )
