(ns day-2
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/2

;; for each GAME, we get a HANDFUL of CUBES 
;; each Cube has one colors and possible colors are :
;; - red
;; - green 
;; - blue
;; Given a list of HANDFUL and given the number of cubes per color
;; in the bag, remove all impossible handful.

(def sample-input-1 "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

;; for each game we want to get :
;; - its number
;; - its handful
;; Then remove all impossible handul and sum up the remaining games ids

;; After day 1, let's keep on using regex to build internal
;; representation of a game

(comment
  ;; we need to extract game id

  (last (re-matches #"Game (\d+):.*"
                    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"))

  (defn game-id [handful]
    (->> handful
         (re-matches #"Game (\d+):.*")
         last
         Integer/parseInt))

  (game-id "Game 28: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")

  ;; and for each color, the total count
  (def handful-match (re-seq #"((\d+) (green|red|blue))"
                             "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"))

  ;; create a map where key is color and val is total count of this color in the handful
  ;; for example :
  ;; {"blue" 6, "green" 6, "red" 1}

  (reduce (fn [acc match]
            (let [color (last match)
                  n     (Integer/parseInt (last (butlast match)))]
              (update acc color (fnil (partial + n) 0)))) {} handful-match)

  (defn distribution-per-color [handful]
    (let [handful-match (re-seq #"((\d+) (green|red|blue))" handful)]
      (reduce (fn [acc match]
                (let [color (last match)
                      n     (Integer/parseInt (last (butlast match)))]
                  (update acc color (fnil (partial + n) 0)))) {} handful-match)))

  (distribution-per-color "Game 28: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")


  ;; now how can we validate a possible handful based on this map representation that we have?

  ;; assuming this is the bag content
  (def bag {"red"    12
            "green"  13
            "blue"   12})

  ;; and this is the color distribution to test
  (def handful-to-test [1 {"red"    11
                           "green"  13
                           "blue"   14}])

  ;; create a predicate to return TRUE if the handful is impossible for
  ;; the given bag-content

  (defn impossible-handful [bag]
    (fn [[_game-id color-distrib]]
      (some (fn [[color-name color-count]]
              (> color-count (get bag color-name))) color-distrib)))

  (remove (impossible-handful bag) [handful-to-test])

  ;; ok, it seems we have everything we need to solve this one.
  ;; let's put our function in the current ns and try
  ;;
  )

(defn game-id
  "Given a handful, return the game id"
  [handful]
  (->> handful
       (re-matches #"Game (\d+):.*")
       last
       Integer/parseInt))

(game-id "")
(defn distribution-per-color
  "Given a handulf returns a map where keys are colors and values are number of times
  the color appears in the handful"
  [handful]
  (let [handful-match (re-seq #"((\d+) (green|red|blue))" handful)]
    (reduce (fn [acc match]
              (let [color (last match)
                    n     (Integer/parseInt (last (butlast match)))]
                (update acc color (fnil (partial + n) 0)))) {} handful-match)))

(defn impossible-handful
  "Given bag content, returns a predicate that returns TRUE if the handful cannot be obtained
  given the provided bag color distribution"
  [bag]
  (fn [[_game-id color-distrib]]
    (some (fn [[color-name color-count]]
            (> color-count (get bag color-name))) color-distrib)))

(defn solution-1 [handful-list bag]
  (let [impossible-handful? (impossible-handful bag)]
    (->> handful-list
         (map (fn [handful]
                (vector (game-id handful) (distribution-per-color handful))))
         (remove impossible-handful?)
         (map first)
         (reduce +))))

(comment

;; the sample input gives correct result ✔
  (solution-1 (s/split-lines sample-input-1) {"red"   12
                                              "green" 13
                                              "blue"  14})

;; trying with puzzle input
  (solution-1 (s/split-lines (slurp "resources/day_2.txt")) {"red"   12
                                                             "green" 13
                                                             "blue"  14})
;; 😭 ... wrong result (254)
;; let's analyze why is that. First, create some tests
;; humm ok i found what the proble is : I didn't read the description of the problem correctly
;; Each handful is composed of a "semicolon-separated list of subsets of cubes "
;; and the test on the numnber of count revealed apply on each subset independently and NOT on the wholme handful 
;; 🤓 ... next time I'll pay more attention to what I read.

;; better is to start all over again in day_2_read_again.clj


  ;;
  )


