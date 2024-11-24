(ns day-5
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/5

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

;; We have to send a seq of number into several mapping and choose among all the output
;; the smallest one.
;; Each mapping follows the same algo.
;; 
;; A map is made of lines, each one made of 3 numbers
;; - destination range start
;; - source range start
;; - range length
;; Note that destination and source ranges have the same length
;;
;; If the number to map is in the source range start, return its corresponding value in the destination
;; range, otherwise return the input number unchanged
;; 
;; Before parsing the input data, let's make some experiments 

(comment

  ;; This hard coded snippet does the trick
  (let [[dest-start source-start range-len]  [52 50 48]
        input-val 13]
    (if (> (+ source-start range-len) input-val (dec source-start))
      (+ dest-start (- input-val source-start))
      input-val))

  ;; Let's extract a function 
  (defn in-source-range [v [_dest-start source-start range-len]]
    (when (> (+ source-start range-len) v (dec source-start))
      [_dest-start source-start range-len]))

  (in-source-range 53 [52 50 48])
  (in-source-range 31 [52 50 48])

  (defn map-to-dest [v [dest-start source-start _range-len]]
    (+ dest-start (- v source-start)))

  (map-to-dest 53 [52 50 48])

  ;; Now if a map is represented as a seq of [dest-start source-start range-len] 
  ;; we could apply mapping to input value v like this:

  (defn source->dest [v map-ranges]
    (if-let [winning-range (some #(in-source-range v %) map-ranges)]
      (map-to-dest v winning-range)
      v))

  ;; test again with sample input :

  (source->dest 79 [[50 98 2]
                    [52 50 48]])

  (source->dest 14 [[50 98 2]
                    [52 50 48]])

  (source->dest 55 [[50 98 2]
                    [52 50 48]])

  (source->dest 13 [[50 98 2]
                    [52 50 48]])
  ;; 👍 

  ;; It is now time to think about parsing the input string to create a 
  ;; seq of maps
  ;; First extraxct the seeds: Applying following on the first line only
  (map #(Integer/parseInt %) (re-seq #"\d+" (first (s/split-lines sample-input))))

  ;; now starting from line 2, extract all maps
  (map #(re-seq #"\d+" %) (rest (s/split-lines sample-input)))

  (->> sample-input
       s/split-lines
       rest
       (map #(re-seq #"\d+" %))
       (map #(if (nil? %)
               nil
               (map (fn [s]
                      (Integer/parseInt s)) %)))
       (partition-by nil?)
       (filter first))
  ;; It seems we have everything to solve part 1
  ;; Let's clean up and create nice functions for this
  )

(defn in-source-range [v [_dest-start source-start range-len]]
  (when (> (+ source-start range-len) v (dec source-start))
    [_dest-start source-start range-len]))

(defn map-to-dest [v [dest-start source-start _range-len]]
  (+ dest-start (- v source-start)))

(defn source->dest [map-ranges v]
  (if-let [winning-range (some #(in-source-range v %) map-ranges)]
    (map-to-dest v winning-range)
    v))

(defn create-maps [input]
  (->> input
       s/split-lines
       rest
       (map #(re-seq #"\d+" %))
       (map #(if (nil? %)
               nil
               (map (fn [s]
                      (Integer/parseInt s)) %)))
       (partition-by nil?)
       (filter first)))

(defn create-seeds [input]
  (map #(Integer/parseInt %) (re-seq #"\d+" (first (s/split-lines input)))))

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
  ;; => 35 excellent ! this is the expected result 

  ;; ...adn now ...
  (solution-1 (slurp "resources/day_5.txt"))
  ;; 
  ;; ; Error printing return value (NumberFormatException) at java.lang.NumberFormatException/forInputString (NumberFormatException.java:65).
  ;; For input string: "3640772818"
  ;;
  ;; 💥 boum !! 

  ;; This is probably because this throws: 
  (Integer/parseInt "3640772818")

  ;; The puzzle input data contains number much more big than the sample inputs

  ;; We should have used : 
  (biginteger "3640772818")
  ;;
  )

(defn create-maps-1 [input]
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

(defn create-seeds-1 [input]
  (map #(biginteger %) (re-seq #"\d+" (first (s/split-lines input)))))

(defn solution-1-b [input]
  (let [seeds (create-seeds-1 input)
        maps  (create-maps-1 input)]
    (->> maps
         (reduce (fn [acc a-map]
                   (map #(source->dest a-map %) acc)) seeds)
         (reduce min))))

(comment
  ;; test on sample input
  (solution-1-b  sample-input)
  ;; => 35N still ok

  ;; ...adn now ...
  (solution-1-b (slurp "resources/day_5.txt"))
  ;; => 214922730N Huraa ! another ⭐
  )


;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; We must now consider the Seeds line, not as a list of seeds, but as a list of pairs describing seeds range
;; The first number is the start of the range, the second the length of the range.

;; 🛑 looking at puzzle inputs values I suspect an issue !
;; first pair is 3640772818 104094365 😮
;; This range contains a lot of seeds !! We may need to do some optimisation here otherwise we'll
;; wait for days before getting a result.

;; Let's ignore this potential issue for now.
;; So, this time we will create a list of sseds following new rules.


(comment
  ;; First range of the sample input :
  (range 79N (+ 79N 14N))

  (partition 2 [1 2 5 6])


  (defn create-seeds-2 [input]
    (->> (map #(biginteger %) (re-seq #"\d+" (first (s/split-lines input))))
         (partition 2)
         (mapcat (fn [[start len]]
                   (range  start (+ start len))))))

  (create-seeds-2 "seeds: 79 14 55 13")
  ;; with sample input : 
  (def puzzle-input "seeds: 3640772818 104094365 1236480411 161072229 376099792 370219099 1590268366 273715765 3224333694 68979978 2070154278 189826014 3855332650 230434913 3033760782 82305885 837883389 177854788 2442602612 571881366")

  (create-seeds-2 puzzle-input) ;; 💥 boom ... evaluating this form just hang the computer : too many values
  ;; first range contains 104 094 365 seeds .. more than 104 millons !!

  ;; couting all seeds in the range below takes 8 seconds
  (time (count (create-seeds-2 "seeds: 3640772818 104094365")))

  ;; Let's see what happen when we map this huge amount of seeds
  )

(defn create-seeds-2 [input]
  (->> (map #(biginteger %) (re-seq #"\d+" (first (s/split-lines input))))
       (partition 2)
       (mapcat (fn [[start len]]
                 (range  start (+ start len))))))

(defn solution-2 [input]
  (let [seeds (create-seeds-2 input)
        maps  (create-maps-1 input)]
    (->> maps
         (reduce (fn [acc a-map]
                   (map #(source->dest a-map %) acc)) seeds)
         (reduce min))))

(comment

  ;; First check if the result is correct with sample inputs
  (solution-2 sample-input)
  ;; yes, it is 46 as expected

  ;; Now let's play with a reduced set of seeds and the puzzle input data

  #_(solution-2 (slurp "resources/day_5_reduced.txt"))

  ;; Foooorget it !! 😭 .... take more than 10 minutes on my PC so there is no way
  ;; we'll get a solution, with this kind of algorithm from the part 1.

  ;; We need to think about another way ...

  ;; Why not try to see if we can work not with values, but with ranges ? 

  ;; Going back to sample input: 79 14
  ;; 
  ;; - seeds range [79 92] with  92 = (+ 79 (dec 14))) - (+ source-start (dec source-end))
  ;;
  ;; map 1 ------------------------------------------- seed-to-soil
  ;;  [98 99] => [50 52] - dest = source - 48
  ;;  [50 97] => [52 99] - dest = source + 2
  ;; => IN : proecess Range [79 92] :
  ;; - does not overlap with [98 99] : (no change)
  ;; - is included in [50 97] : the input interval should be translated +2
  ;; <== OUT [81 94]

  ;; map 2 ------------------------------------------ soil-to-fertilizer
  ;; [15 51] => [0 ... don't care] - dest = source -15   (15 = dest-start - source-start)
  ;; [52 53] => [37 .. don't care] - dest = source -15 
  ;; => IN : [81 94]
  ;; - [15 51] - no overlap
  ;; - [52 53] - no overlap
  ;; <== OUT [81 94]

  ;; map 3 ------------------------------------------- fertilizer-to-water
  ;; [53 60] => [49 ...] - dest = source - 4
  ;; [11 52] => [0  ...] - dest = source - 11
  ;; [0   6] => [42 ...] - dest = source + 42
  ;; [7  10] => [57 ...] - dest = source + 50
  ;; ==> IN [81 94]
  ;; no start range match
  ;; <== OUT [81 94]

  ;; map 4 -------------------------------------------- water-to-light
  ;; [18 24] => [88 ...] - dest = source + 70
  ;; [25 94] => [18 ...] - dest = source - 7
  ;; ==> IN [81 94]
  ;; - first does not match
  ;; - match ! 
  ;; <== OUT [74 87]

  ;; map 5 -------------------------------------------- light-to-temperature
  ;; [77 99] => [45 ...] => dest = source - 32
  ;; [45 63] => [81 ...] => dest = source + 36
  ;; [64 76] => [68 ...] => dest = source + 4
  ;; ==> INT [74 87]
  ;; - partial match 
  ;;      [74 76] no modified
  ;;      [77 87] -32 => [45 55] - mapping done, added to OUT
  ;; - range 2 : [74 76] no match
  ;; - range 3 : [74 76] is included in [64 76]
  ;;      [74 76] +4 => [78 80] added to OUT
  ;; <== OUT ( [45 55]  [78 80] )

  ;; map 6 --------------------------------------------- temperature-to-humidity
  ;; [69 69] => [70  ] => dest = source + 1
  ;; [0  68] => [1 ..] => dest = source + 1
  ;; ==> INT ( [45 55]  [78 80] )
  ;; - slice 1 : no match for any of 2 input ranges
  ;; - slice 2 : range [45 55] is included in [0 68]
  ;;      [45 55] => [46 56]
  ;; <== OUT ( [46 56]  [78 80] )

  ;; map 7 ---------------------------------------------- humidity-to-location
  ;; [56 92] => [60  ...] => dest = source + 4
  ;; [93 96] => [56  ...] => dest = source - 37
  ;; ==> IN  ( [46 56]  [78 80] )
  ;; - slice 1:
  ;;     - partial match for [46 56]
  ;;             [46  55] : no change
  ;;             [56] => [60]
  ;;     - full match for [78 80] included in [56 92]
  ;;            [78 80] => [82 84]
  ;; - slice 2: no match
  ;; <== OUT [46 55] [60] [82 84]
  ;; 
  ;; Lowest location is 46 👍
  ;; This is the expected result for sample input

  ;; Running this algo "by hand" helps to identify pieces of logic and names

  ;; A map in the Almanac is a list of range shifting rules. Each shifting-rule is made of:
  ;; - start: value that starts  the source range
  ;; - end : values that ends the source range
  ;; - shift-v : value to add to shift from source to dest range
  )

(defn create-shifting-rule [[dest-start source-start range-len]]
  (let [source-end (+ source-start (dec range-len))
        shift-v    (- dest-start source-start)]
    (vector [source-start source-end] shift-v)))

(comment

  (create-shifting-rule [50 98 2])

  ;; A range is a 2 items vector [start end]
  ;; A range has a length of 1 is start = end
  )

(defn create-range [[start len]]
  (vector start (+ start (dec len))))

(comment

  (create-range [79 14])
  ;; Comparing 2 ranges, we may have to deal with following cases

  ;; case 1 : no overlap
  ;; A: ---------------------------[.......]-------------------------
  ;; B: ----------[.......]------------------------------------------

  ;; case 2 : included
  ;; A : ---------------------------[.......]------------------------
  ;; B1: ---------------------------[.......]------------------------
  ;; B2: ----------------------------[.....]-------------------------

  ;; case 3 : left overlap
  ;; A : ---------------------------[.......]------------------------
  ;; B : ---------------------[.......]------------------------------

  ;; case 4 : right overlap
  ;; A : ---------------------------[.......]------------------------
  ;; B : -------------------------------[.......]--------------------

  ;;
  )

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

(comment
  (included? [1 1] [1 1])
  (included? [1 6] [1 5])

  ;; Then we will need a function to left and another for right overlap
  ;; 🛑 wait ! .. Couldn't we use some of the function in the set namespace ? 

  ;; No no, the problem is that set functions (like 'intersection') work on sets, where all values
  ;; are stored, and we don't want that (because there are too much values), we want to work on ranges.

  ;; ok, nevermind.
  ;; Let's continue...
  )


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

(comment
  ;; Given a range and a shifting rule, let's create a function that 
  ;; returns the result of applying the rule to the range. The result will be a seq
  ;; of ranges.

  ;;
  )

(defn apply-shift-rule [[cur-start cur-end :as current-range]
                        [source-range shift-val]]
  (cond
    (included? current-range source-range)       (do
                                                   (prn "included")
                                                   {:remain []
                                                    :mapped (mapv #(+ % shift-val) current-range)})

    (left-overlap? current-range source-range)   (do
                                                   (prn "left")
                                                   (let [outside [cur-start
                                                                  (dec (first source-range))]
                                                         inside  [(first source-range)
                                                                  (min cur-end
                                                                       (second source-range))]]
                                                     {:remain [outside]
                                                      :mapped (mapv #(+ % shift-val) inside)}))

    (right-overlap? current-range source-range)  (prn "right")
    :else                                  (do
                                             (prn "none")
                                             current-range)))

(comment
  ;; After working on the apply-shift-rule function , I think it would be much simpler to
  ;; have an 'intersection' function that, given 2 ranges, return the sub-range in common and the otherones

  ;; Sa = Start A, Ea = End A, Sb = Start B, Eb = End B

  ;; case 1 : no overlap
  ;; A : ---------------------------[.......]-------------------------
  ;; B1: ----------[.......]------------------------------------------ Eb < Sa  
  ;; B2: ----------------------------------------[.......]------------ Sb > Ea
  ;;                                                     condition ==>  Eb < Sa  OR Ea < Sb
  ;;                     split : mapped  = []
  ;;                             remains = [Sb  Eb]

  ;; case 2 : included
  ;; A : ---------------------------[.......]------------------------ 
  ;; B1: ---------------------------[.......]------------------------ Sa = Sb AND Ea = Eb
  ;; B2: ----------------------------[.....]------------------------- Sa < Sb AND Ea > Eb
  ;;                                                    condition ==> Sa <= Sb AND Ea >= Eb 
  ;;                     split : mapped = [Sb Eb]
  ;;                            remains = []

  ;; case 3 : left overlap
  ;; A : ---------------------------[.......]------------------------
  ;; B1 : ---------------------[.......]----------------------------- Sb < Sa AND Eb <= Ea   
  ;;                     split : mapped  = [Sa Eb]
  ;;                             remains = [Sb (Sa - 1)]
  ;; B2 : ---------------------[................]-------------------- Sb < Sa AND Eb > Ea
  ;;                     dplit : mapped  = [Sa Ea]
  ;;                             remains = [Sb (Sa -1)] [(Ea + 1) Eb] 

  ;; case 4 : right overlap
  ;; A : ---------------------------[.......]------------------------
  ;; B1 : -------------------------------[.......]------------------- Sb >= Sa  AND Eb > Ea 
  ;;                    split : mapped  = [Sb Ea]
  ;;                            remains = [(Ea + 1) Eb]
  ;; B2 : ---------------------[.................]------------------- Sb < Sa AND Eb > Ea
  ;;                    split : mapped  = [Sa Ea]                        (same as previous) 
  ;;                            remains = [Sb (Sa -1)]  [(Ea + 1) Eb]    (same as previous)

  ;; Let's create this 'intersection' function
  )


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

(defn apply-shift-rule-1
  "Applies the given shifting rule to the given range. Returns a map :
   - `:mapped` : range that could be mapped. Empty vector when no mapping was possible
   - `:remain` : vector of ranges that could not be mapped by the rule"
  [current-range [source-range shift-val :as rule]]
  (let [[common-range remaining-ranges] (intersection current-range source-range)]
    {:mapped (mapv #(+ % shift-val) common-range)
     :remain remaining-ranges}))



;; What if instead of only one range to map, we have a seq of ranges ?
;; First a small helper function that (hopefully) will clarify ...

(defn update-if-not-empty [current v f]
  (if (seq v)
    (f current v)
    current))

(comment
  (update {:a []} :a update-if-not-empty nil conj)
  (update {:a []} :a update-if-not-empty [2] conj)
  (update {:a []} :a update-if-not-empty [2] into)

  ;;
  )

(defn apply-rule-on-ranges-1 [rule {:keys [remain mapped] :as initial-state}]
  (reduce (fn [state cur-range]
            (let [result-m (apply-shift-rule-1 cur-range rule)]
              (-> state
                  (update :remain update-if-not-empty (:remain result-m) into)
                  (update :mapped update-if-not-empty (:mapped result-m) conj))))
          {:remain [] :mapped mapped}
          remain))

(comment
  (apply-rule-on-ranges-1 [[29 94]  -7] {:remain [] :mapped []})
  (apply-rule-on-ranges-1 [[29 94]  -7] {:remain [[1 25] [20 100]] :mapped []})
  (apply-rule-on-ranges-1 [[29 94]  -7] {:remain [[1 110]] :mapped []})
  ;;
  )

;; We can apply a rule to a sed of ranges, and we have now to do that for all rules in a map

(defn apply-mapping [rules initial-state]
  (reduce (fn [state cur-rule]
            (apply-rule-on-ranges-1 cur-rule state))
          initial-state
          rules))

(comment
  (apply-mapping [[[5 10] 2] [[20 30] 10]] {:remain [[1 6] [22 28]] :mapped []})
  ;;
  )

;; Before being able to test this function  we must change the way we used to parse input
;; in the first part. Now each shifting rule in a map is a 2 items vector.

(defn create-maps-2 [input]
  (->> input
       create-maps-1
       (map (fn [rules]
              (map create-shifting-rule rules)))))
(comment
  (create-maps-2 sample-input)
  ;;
  )

;; Same for input seeds which must now be considered as a set of ranges
(defn create-initial-ranges [input]
  (->> input
       s/split-lines
       first
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (partition 2)
       (map create-range)))

(comment
  (create-initial-ranges sample-input)

  ;; Now we should have enough to run manually the complete process with sample inputs

  (def almanac (create-maps-2 sample-input))
  (def initial-ranges (create-initial-ranges sample-input))

  (def after-mapping (reduce (fn [state almanac-map]
                               (if (empty? (:remain state))
                                 (reduced state)
                                 (let [{:keys [remain mapped] :as result} (apply-mapping almanac-map state)]
                                   (prn state)
                                   (prn almanac-map)
                                   (prn result)
                                   (prn "-----------------------")
                                   {:remain (into remain mapped)
                                    :mapped []})))
                             {:remain initial-ranges :mapped []}
                             almanac))

  (apply min (flatten (:remain after-mapping)))
  ;; => 46 this is the correct answer

  ;; Let's create a function to solve part 2 (and cross finger)




  ;;
  )

(defn solution-2-b [input]
  (let [almanac (create-maps-2 input)
        initial-ranges (create-initial-ranges input)
        final-state (reduce (fn [state almanac-map]
                              (if (empty? (:remain state))
                                (reduced state)
                                (let [{:keys [remain mapped] :as result} (apply-mapping almanac-map state)]
                                  {:remain (into remain mapped)
                                   :mapped []})))
                            {:remain initial-ranges :mapped []}
                            almanac)]
    (apply min (flatten (:remain final-state)))))

(comment

  ;; still working ok with sample inputs
  (solution-2-b sample-input)
  ;; => 46

  ;; and now ... trying with puzzle inputs 🎺🎺🎺

  (solution-2-b (slurp "resources/day_5.txt"))
  ;; arg !
  ;; ; Execution error (NumberFormatException) at java.lang.NumberFormatException/forInputString (NumberFormatException.java:65).
  ;; For input string: "3640772818"
  ;; This number is in fact the first of my puzzle input and obiously I have forgotten
  ;; to deal with *biginteger* when creating the initial range.
  ;;
  )

(defn create-initial-ranges-1 [input]
  (->> input
       s/split-lines
       first
       (re-seq #"\d+")
       (map #(biginteger %))
       (partition 2)
       (map create-range)))

(defn solution-2-c [input]
  (let [almanac (create-maps-2 input)
        initial-ranges (create-initial-ranges-1 input)
        final-state (reduce (fn [state almanac-map]
                              (if (empty? (:remain state))
                                (reduced state)
                                (let [{:keys [remain mapped] :as result} (apply-mapping almanac-map state)]
                                  {:remain (into remain mapped)
                                   :mapped []})))
                            {:remain initial-ranges :mapped []}
                            almanac)]
    (apply min (flatten (:remain final-state)))))

(comment
  ;; Thie biginteger is fixed, try again 
  
  (solution-2-c sample-input)
  ;; => 46 ok ok 

  ;; and now ... 

  (solution-2-c (slurp "resources/day_5.txt"))
  ;; => 148041808 
  ;; 🎉^yes yes yes !! one ⭐ more

  ;; This has been a long trip since line 1. The solution for part2 was not so obvious as I though
  ;; and it took me a while to figure it out.
  ;; To leave this day claen, I'll summarize the results in namespace day-5-final
  )