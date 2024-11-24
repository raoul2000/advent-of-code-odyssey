(ns day-14
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]))

;; part 1 ============================

(def test-data "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defn update-insertion-rules-map [m s]
  (->> (re-matches #"(..) -> (.)" s)
       rest
       (apply (partial assoc m))))

(defn parse-data [s]
  (let [[polymer-template _ insertion-rules] (->> (str/split-lines s)
                                                  (partition-by #{""}))]
    [(first polymer-template)
     (reduce update-insertion-rules-map {} insertion-rules)]))

(defn create-pairs [s]
  (map #(apply str %) (partition 2 1 s)))

(defn do-step-fn [insertion-rules]
  (fn [polymer]
    (let [result (->> (create-pairs  polymer)
                      (reduce #(conj %1 (str (first %2) (get insertion-rules %2))) [])
                      (apply str))]
      (str result (last  polymer)))))

(defn calculate-score [s]
  (->> (frequencies s)
       vals
       (apply (juxt max min))
       (apply -)))

(defn solve-part-1 [s step-count]
  (let [[polymer insertion-rules] (parse-data s)
        step-fn                   (do-step-fn insertion-rules)]
    (->> (iterate step-fn polymer)
         (take (inc step-count))
         last
         calculate-score)))

(comment
  (solve-part-1 test-data 10)
  ;; => 1588
  (solve-part-1 (slurp "./resources/puzzle_14.txt") 10))
  ;; => 3118


;; part 2 =============================================
;; Just like day 6, we are facing an exponential computation time increase
;; With 20 steps, solution to part-1 take 3.4s. For 1 more step (21) we reach 6.6s
;; some metrics on test-data :
;; 10 : "Elapsed time: 15.0679 msecs"
;; 1588
;; 15 : "Elapsed time: 119.018 msecs"
;; 56892
;; 20 : "Elapsed time: 3466.4279 msecs"
;; 1961318
;; 21 : "Elapsed time: 6609.8251 msecs"
;; 3942739
;;
;; We must find a way to parallelize computation see if we can get better results
;; in terms of computation time.

(defn reducef [insertion-rules]
  (fn
    ([] [])
    ([result item]
     (conj result (str (first item) (get insertion-rules item))))))

(defn combinef
  ([] [])
  ([a b]
   (into a b)))

(defn better-do-step-fn [insertion-rules]
  (fn [polymer]
    (let [result (->> (create-pairs  polymer)
                      (into [])
                      (r/fold combinef (reducef insertion-rules))
                      (apply str))]
      (str result (last  polymer)))))


(defn solve-part-2 [s step-count]
  (let [[polymer insertion-rules] (parse-data s)
        step-fn                   (better-do-step-fn insertion-rules)]
    (->> (iterate step-fn polymer)
         (take (inc step-count))
         last
         calculate-score)))

(comment

  (time (solve-part-2 test-data 10))
  ;; 10 : "Elapsed time: 4.6831 msecs / 15.0679 msecs"
  ;; 1588
  ;; 15 : "Elapsed time:75.5656 msecs / 119.018 msecs"
  ;; 56892
  ;; 20 : "Elapsed time: 2371.2025 msecs / 3466.4279 msecs"
  ;; 1961318
  ;; 21 : "Elapsed time:  4665.855501 msecs / 6609.8251 msecs"
  ;; 3942739
  ;; 25 : "Elapsed time:  174303.700901 msecs / no metrics"
  ;; 64726890

  ;; using parallel reduce (with r/fold) improve computation time but 
  ;; not enough to be acceptable
  ;; ... just like for day 6 we must find another solution
  )


;; another option is to consider the polymer as a seq of characters
;; and avoid string/seq conversion

(defn rule-matcher [insertion-rules]
  (memoize ;; save some time with memoize ?
   (fn [a b]
     (if (nil? a)
       [b]
       (if-let [ins (get insertion-rules (str a b))]
         [a ins b]
         [a b])))))

(defn do-step-fn-a [assemble]
  (fn [polymer]
    (->> polymer
         (reduce (fn [result c]
                   (apply (partial conj result) (assemble (last result) c))) []))))

(defn solve-part-2a [s step-count]
  (let [[polymer insertion-rules] (parse-data s)
        step-fn                   (do-step-fn-a (rule-matcher insertion-rules))]
    (->> (iterate step-fn polymer)
         (take (inc step-count))
         last
         calculate-score)))

(comment
  (time (solve-part-2a test-data 7))
  ;; "Elapsed time: 189.6861 msecs"
  ;; ok stop now : worst than before
  )

(defn solve-part-2b [s step-count]
  (let [[polymer insertion-rules] (parse-data s)
        step-fn                   (do-step-fn insertion-rules)]
    (->> (iterate step-fn polymer)
         (take (inc step-count))
         last
         calculate-score)))

(comment
  (time (solve-part-2b test-data 10))
  ;; "Elapsed time: 6.2774 msecs"
  ;; 1588

  (time (solve-part-2b test-data 15))
  ;; "Elapsed time: 103.4746 msecs"
  ;; 56892

  (time (solve-part-2b test-data 20))
  ;; "Elapsed time: 2914.4404 msecs"
  ;; 1961318

  (time (solve-part-2b test-data 21))
  ;; "Elapsed time: 5532.4629 msecs"
  ;; 3942739

  ;; nope :(
  ;; we are still spending too much time !
  )

;; ok this one is tricky. Let's find a completely different way
;; to solve this problem.

;; initial polymer
(def initial-polymer "NNCB")

;; create initial pair list
(def initial-pair-coll (map (partial into []) (partition 2 1 initial-polymer)))

;; store pairs in a map where k is the pair and v the occurence
;; example :
;; { "NN" 1
;;   "NC" 2
;;   etc...
;; }
;; let's see that on an example: statr with NNCB which produce the following map:
;;
;; { "NN" 1
;;   "NC" 1
;;   "CB" 1}
;; The first pair "NN" is replaced with 2 new pairs, based on the insertion rules :
;; "NC" and "CN".Result is :
;;
;; { "NC" 1  <-
;;   "CN" 1  <-
;; }
;; Now let's work on "NC" which splits into "NB" and "BC". 
;; { "NC" 1
;;   "CN" 1
;;   "NB" 1  <-
;;   "BC" 1  <-
;; }
;; and to end this step, let's work on "CB" which splits into "CH" and "HB"
;; { "NC" 1
;;   "CN" 1
;;   "NB" 1
;;   "BC" 1
;;   "CH" 1 <-
;;   "HB" 1 <-
;; }
;; ====> This is our map after step 1. Let's go step 2
;;
;; "NC" -> "NB" "BC"
;; { "NB" 1 <-
;;   "BC" 1 <-
;; }
;; "CN" -> "CC" "CN"
;; { "NB" 1
;;   "BC" 1
;;   "CC" 1 <-
;;   "CN" 1 <-
;; }
;; "NB" => "NB" "BB"
;; { "NB" 2 <-
;;   "BC" 1
;;   "CC" 1
;;   "CN" 1
;;   "BB" 1 <-
;; }
;; "BC" -> "BB" "BC"
;; { "NB" 2
;;   "BC" 2 <-
;;   "CC" 1
;;   "CN" 1
;;   "BB" 2 <-
;; }
;; "CH" -> "CB" "HB"
;; { "NB" 2
;;   "BC" 2
;;   "CC" 1
;;   "CN" 1
;;   "BB" 2
;;   "CB" 1 <-
;;   "HB" 2 <-
;; }
;; "HB" -> "HC" "CB"
;; { "NB" 2
;;   "BC" 2
;;   "CC" 1
;;   "CN" 1
;;   "BB" 2
;;   "CB" 2 <-
;;   "HB" 2 <-
;;   "HC" 1
;; }
;; ====> This is our map after step 1. Let's go step 3
;; "NB" -> "NB" "BB" but as we have 2 pairs "NB" each one will produce 2 pairs
;; { "NB" 2 <-
;;   "BB" 2 <-
;; }
;; "BC" -> "BB" "BC" this one also has a value of 2, so we must add 2 "BB" 
;; and 2 "BC"
;; { "NB" 2
;;   "BB" 2 <-
;;   "BC" 4 <-
;;   "BB" 2
;; }
;; etc ..
;; let's try to implement this.
;; first we need to create the insertion rules map

(defn create-val [k v]
  [(str (first k) v)
   (str v (last k))])

(defn insertion-rules-map-reducer [m s]
  (let [[k v] (->> (re-matches #"(..) -> (.)" s)
                   rest)]
    (assoc m k (create-val k v))))

(defn build-insertion-rules-map
  "Creates and returns an insertion rule map from a seq of rules.
   Example:
   ```
   (build-insertion-rules-map [\"CH\" -> \"B\"])
   => {\"CH\" [\"CB\" \"BH\"]}
 
   ```
   " [rules]
  (reduce insertion-rules-map-reducer {} rules))

;; now we must create the first pair map given the initial polymer

(defn create-initial-pairs-map
  "Returns a pair map given a polymer.
   Example:
   ```
   (create-pairs-map \"ABCDE\")
   {\"AB\" 1, \"BC\" 1, \"CD\" 1, \"DE\" 1}                         
   ```
   " [polymer]
  (->> (partition 2 1 polymer)
       (map #(vector (apply str %) 1))
       (into {})))

(defn parse-data-2 [s]
  (let [[polymer-template _ insertion-rules] (->> (str/split-lines s)
                                                  (partition-by #{""}))]
    [((comp create-initial-pairs-map first) polymer-template)
     (build-insertion-rules-map insertion-rules)]))



;; And now the main function to apply an insertion step on a given pair-map
;; and return the result



(defn insert-element [insertion-rules]
  (fn [m [element cnt]]
    (if-let [extra-elements (get insertion-rules element)]
      ;; insertion occurs
      (reduce (fn [m v]
                (update m v #(if (nil? %) cnt (+ cnt %)))) m extra-elements)
      ;; no insertion
      (assoc m element cnt))))

(comment
  (reduce (fn [m v]
            (update m v #(if (nil? %) 2 (+ 2 %)))) {:c 5} [:a :b])
  ;;
  )

(defn apply-step [[m insertion-rules]]
  [(reduce (insert-element insertion-rules) {"B" 1} m)
   insertion-rules])

(comment
  (apply-step [{"AB" 2}
               {"AB" ["AC" "CB"]}])

  (apply-step [{"NN" 1}
               {"AB" ["AC" "CB"]}])

  (let [[polymer ins-rules] (parse-data-2 test-data)]
    (->> (apply-step [polymer ins-rules])
         apply-step
         ;;apply-step
         ;;apply-step
         ))
  ;;
  )
;; Eventually, a function to compute the final score
;; from a pair map
;; {"NB" 2, "BC" 2, "CC" 1, "CN" 1, "BB" 2, "CB" 2, "BH" 1, "HC" 1}

(defn calculate-score-2 [pair-map]
  (->> (map (fn [[pair cnt]] (repeat cnt (first pair))) pair-map)
       flatten
       (apply str)
       ;;count
       frequencies
       vals
       (apply (juxt max min))
       (apply -)))

(comment
  (calculate-score-2 {"AB" 2 "CD" 3})
  ;;
  )

;; and solve

(defn solve-part-2c [s step-count]
  (let [[polymer ins-rules] (parse-data-2 s)]
    (->> (iterate apply-step [polymer ins-rules])
         (take  (inc step-count))
         last
         first
         ;;calculate-score-2
         )))

(defn solve-part-2d [s step-count]
  (let [[polymer ins-rules] (parse-data-2 s)
        final-poly-map (loop [[_ & remaining] (range 0 (inc step-count))
                               poly-map polymer]
                          (if (empty? remaining)
                            poly-map
                            (recur remaining
                                   (first (apply-step [poly-map ins-rules])))))]
    ;;final-poly-map
    (calculate-score-2 final-poly-map)
    ))

(comment
  (solve-part-2c test-data 1)
  (solve-part-2c test-data 2)
  (solve-part-2c test-data 4)
  (solve-part-2c test-data 10)

  ;; => 1588

  (solve-part-2d test-data 1)
  (solve-part-2d test-data 2)
  (solve-part-2d test-data 3)
  (solve-part-2d test-data 4)
  (solve-part-2d test-data 10)
  (solve-part-2d (slurp "./resources/puzzle_14.txt") 10)
  (solve-part-2d (slurp "./resources/puzzle_14.txt") 20)


  (solve-part-2c (slurp "./resources/puzzle_14.txt") 10)
  (time (solve-part-2c (slurp "./resources/puzzle_14.txt") 20))
  ;; "Elapsed time: 9741.3214 msecs"
  ;; 3745243

  (time (solve-part-2d (slurp "./resources/puzzle_14.txt") 20))

  (time (solve-part-2c (slurp "./resources/puzzle_14.txt") 21))
  ;; "Elapsed time: 19402.9067 msecs"
  ;; 7540881


  ;;
  )




