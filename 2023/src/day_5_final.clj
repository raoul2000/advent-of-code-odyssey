(ns day-5-final
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/5

;; This is a summarized version of days 5 solutions. For the complete story, please refer to ns day-5

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sample-input "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn in-source-range [v [_dest-start source-start range-len]]
  (when (> (+ source-start range-len) v (dec source-start))
    [_dest-start source-start range-len]))

(defn map-to-dest [v [dest-start source-start _range-len]]
  (+ dest-start (- v source-start)))

(defn source->dest [map-ranges v]
  (if-let [winning-range (some #(in-source-range v %) map-ranges)]
    (map-to-dest v winning-range)
    v))

(defn create-seeds [input]
  (map #(biginteger %) (re-seq #"\d+" (first (s/split-lines input)))))

(defn create-maps [input]
  (->> input
       s/split-lines
       rest
       (map #(re-seq #"\d+" %))
       (map #(if (nil? %)
               nil
               (map (fn [s]
                      (biginteger s)) %)))
       (partition-by nil?)
       (filter first)))

(defn solution-1 [input]
  (let [seeds (create-seeds input)
        maps  (create-maps input)]
    (->> maps
         (reduce (fn [acc a-map]
                   (map #(source->dest a-map %) acc)) seeds)
         (reduce min))))

(comment
  ;; test on sample input
  (solution-1  sample-input)
  ;; => 35N  ok

  ;; ...adn now ...
  (solution-1 (slurp "resources/day_5.txt"))
  ;; => 214922730N Huraa ! another ⭐
  )


;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't miss the full story in file day_5.clj ... it's a long story.


(defn create-shifting-rule [[dest-start source-start range-len]]
  (let [source-end (+ source-start (dec range-len))
        shift-v    (- dest-start source-start)]
    (vector [source-start source-end] shift-v)))

(defn create-range [[start len]]
  (vector start (+ start (dec len))))

(defn included?
  "Returns TRUE if range 1 in included in range 2.
   ```
   Range 2 : -------------[....]----------
   Range 1 : --------------[..]----------- true
   Range 1 : -------------[....]---------- true
   Range 1 : ----------[....]------------- false
   ```
   "
  [[s1 e1] [s2 e2]]
  (and (<= s2 s1)
       (>= e2 e1)))


(defn left-overlap?
  "Returns TRUE is range 1 overlap left with range 2 :
   
   ```
   Range 2 : -------------[...
   Range 1 : --------[.........
   ```
   "
  [[s1 e1] [s2 e2]]
  (and (< s1 s2)
       (>= e1 s2)))

(defn right-overlap?
  "Returns TRUE is range 1 overlap right with range 2 :
   
   ```
   Range 2 : ......]------------
   Range 1 : ..........]-------
   ```
   "
  [[s1 e1] [_s2 e2]]
  (and (< e2 e1)
       (>= e2 s1)))

(defn intersection
  "Returns a vector where the first item is the range in range-1 included in range-2, and
   as second item, the list of sub ranges in range-1 that do not belong to range-2."
  [[s1 e1 :as range-1] [s2 e2 :as range-2]]
  (cond
    (included? range-1 range-2)
    [range-1 []]

    (and (left-overlap?  range-1 range-2)
         (right-overlap? range-1 range-2))
    [[s2 e2] [[s1 (dec s2)] [(inc e2) e1]]]

    (left-overlap?  range-1 range-2)
    [[s2 e1] [[s1 (dec s2)]]]

    (right-overlap?  range-1 range-2)
    [[s1 e2] [[(inc e2) e1]]]

    :else
    [[] [range-1]]))

(defn apply-shift-rule
  "Applies the given shifting rule to the given range. Returns a map :
   - `:mapped` : range that could be mapped. Empty vector when no mapping was possible
   - `:remain` : vector of ranges that could not be mapped by the rule"
  [current-range [source-range shift-val]]
  (let [[common-range remaining-ranges] (intersection current-range source-range)]
    {:mapped (mapv #(+ % shift-val) common-range)
     :remain remaining-ranges}))


(defn update-if-not-empty [current v f]
  (if (seq v)
    (f current v)
    current))

(defn apply-rule-on-ranges [rule {:keys [remain mapped]}]
  (reduce (fn [state cur-range]
            (let [result-m (apply-shift-rule cur-range rule)]
              (-> state
                  (update :remain update-if-not-empty (:remain result-m) into)
                  (update :mapped update-if-not-empty (:mapped result-m) conj))))
          {:remain [] :mapped mapped}
          remain))

(defn apply-mapping [rules initial-state]
  (reduce (fn [state cur-rule]
            (apply-rule-on-ranges cur-rule state))
          initial-state
          rules))

(defn create-maps-2 [input]
  (->> input
       create-maps
       (map (fn [rules]
              (map create-shifting-rule rules)))))

(defn create-initial-ranges [input]
  (->> input
       s/split-lines
       first
       (re-seq #"\d+")
       (map #(biginteger %))
       (partition 2)
       (map create-range)))

(defn solution-2 [input]
  (let [almanac        (create-maps-2 input)
        initial-ranges (create-initial-ranges input)
        final-state    (reduce (fn [state almanac-map]
                                 (if (empty? (:remain state))
                                   (reduced state)
                                   (let [{:keys [remain mapped]} (apply-mapping almanac-map state)]
                                     {:remain (into remain mapped)
                                      :mapped []})))
                               {:remain initial-ranges :mapped []}
                               almanac)]
    (apply min (flatten (:remain final-state)))))

(comment
  ;; test on sample input
  (solution-2 sample-input)
  ;; => 46 

  ;; and now ... 

  (solution-2 (slurp "resources/day_5.txt"))
  ;; => 148041808 
  ;; 🎉^yes yes yes !! one ⭐ more
  )