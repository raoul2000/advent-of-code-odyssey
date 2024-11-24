(ns day-6
  (:require [clojure.string :as s]
            [clojure.math :as m]))

;; https://adventofcode.com/2023/day/6

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def sample-input "Time:      7  15   30
Distance:  9  40  200")

;; First we'll have to parse input to create a list of integers pairs [t d];
;; - 't' : is the race duration time
;; - 'd' : is the current distance record
;;
;; For a given d and t we must find all x where x*(t - x) > d  with  0 < x < t
;; We must then count how many x where found for each pair, and multiply them all
;; 
;; First hings first : parsing the input

(comment
  (->> (s/split-lines sample-input)
       (map #(->> (re-seq #"\d+" %)
                  (map (fn [n] (Integer/parseInt n)))))
       (apply map vector))
  ;;
  )

(defn parse-input [input]
  (->> (s/split-lines input)
       (map #(->> (re-seq #"\d+" %)
                  (map (fn [n] (Integer/parseInt n)))))
       (apply map vector)))

(comment
  (parse-input sample-input)
  (parse-input (slurp "resources/day_6.txt")))


;; Ok, now we have our pairs of integer, we need to process each one
(defn wins [[t d]]
  (filter (fn [x]
            (> (* x (- t x)) d)) (range 1 (dec d))))

(comment
  (count (wins [7 9]))
  (count (wins [15 40]))
  (count (wins [30 200]))

  ;; It seems we have enough to solve part 1
  )

(defn solution-1 [input]
  (->> input
       parse-input
       (map (comp count wins))
       (reduce *)))

(comment
  ;; test on sample input
  (solution-1  sample-input)
  ;; => 288  ok

  ;; ...adn now ...
  (solution-1 (slurp "resources/day_6.txt"))
  ;; => 1159152 good ! ⭐

  ;; Now, this part 1 was too simple compared to previous days ... 
  ;; The part 2 may be muuch more complex I guess. Let's see
  )

;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We must now consider only one pair, so the parsing operation
;; must be modified to concat each number from line 1 and each numbers from line 2

(comment
  (->> (s/split-lines sample-input)
       (map #(re-seq #"\d+" %))
       (map #(apply str %))
       (map #(Integer/parseInt %)))

  (count (wins [71530 940200]))
  ;; => 71503 This is the expected result for sample input
  ;; By looking at puzzle input we will probably need biginteger 

  ;; Let's try
  ;;
  )

(defn parse-input-2 [input]
  (->> (s/split-lines input)
       (map #(re-seq #"\d+" %))
       (map #(apply str %))
       (map #(biginteger %))))

(comment
  (parse-input-2 sample-input)
  ;;
  )

(defn solution-2 [input]
  (->> input
       parse-input-2
       wins
       count))

(comment
  ;; test on sample input
  (solution-2  sample-input)
  ;; => 71503 ... still good  ok

  ;; ...and now ...
  (solution-2 (slurp "resources/day_6.txt"))
  ;; Yeah, right 🛑 .. too big number, too much computing. Maybe some sort of 
  ;; quantic computer could do it, but my poor PC can't. We must think of a more clever
  ;; way to get the answer

  ;; Basically the pair for our puzzle input is : 
  (parse-input-2 (slurp "resources/day_6.txt"))
  ;; => (58819676 434104122191218) 😮 that's a lot


  ;; Ok so what do we want ?  Find all x for which 
  ;;  x*(time - x) > distance

  ;; let's do some math:

  ;; x*(time - x) > distance
  ;; x*(time - x) - distance > 0
  ;; x*time - x² - distance > 0
  ;; - x² +x*time  - distance > 0

  ;; .. and we have here a quadratic inequality  🤓
  ;; which can be solved if you remember your match classes. If you don't (like me)
  ;; then you'll find plenty of info on the web or youtube.

  (defn solve
    "solve - x^2 + t*x -d > 0
           a*x² + b*x + c > 0  
     a = -1
     b = t
     c = -d
     delta = b² - 4ac
     "
    [t d]
    (let [delta (- (* t t) (* 4 -1 (* -1 d)))
          x1    (/ (- (* -1 t) (Math/sqrt delta)) -2)
          x2    (/ (+ (* -1 t) (Math/sqrt delta)) -2)]
      {:delta   delta
       :x1      x1
       :x2      x2
       :inter   [(min x1 x2) (max x1 x2)]

       :inter2  [(biginteger (m/ceil  (min x1 x2)))
                 (biginteger (m/floor (max x1 x2)))]

       :result (inc (- (biginteger (m/floor (max x1 x2)))
                       (biginteger (m/ceil  (min x1 x2)))))

       ;;
       }))


  ;; let's try with sample inputs
  (:result (solve 71530 940200))
  ;; => 71503 Good

  ;; and now with the puzzle input
  ;; Ah yes, no need to parse and everything, just does it
  ;; manuelly (lazy)
  (:result (solve 58819676N 434104122191218N))
  ;; => 41513103 ....⭐ (one more)
  ;;
  )