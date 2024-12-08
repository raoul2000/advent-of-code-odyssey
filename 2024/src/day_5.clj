(ns day-5
  (:require [clojure.string :as s]
            [clojure.set :refer [intersection]]))

;; https://adventofcode.com/2024/day/5

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rules (page ordering rules)
;;   n | m 
;;        => n MUST be printed BEFORE m
;;        => m MUST be printed after n
;; page-update : pages to produce in each update
;;
;; which updates are already in the right order ?
;; find the middle page number of each update being printed
;; ... and sum then all

(def sample-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
")

;;
;; First let's build a map of ordering rules where
;; - key is a page numnber
;; - value is a pair where
;;        - the first item is a list of page number that should be printed BEFORE Key
;;        - the first item is a list of page number that should be printed AFTER Key
;;
(def empty-rule [#{} #{}])
(defn update-rule-before [[rule-before rule-after] n]
  [(conj rule-before n) rule-after])

(defn update-rule-after [[rule-before rule-after] n]
  [rule-before (conj rule-after n)])

(defn create-rules-map
  "Given puzzle input, returns a map where : 
   
   - key : a page number
   - (first val) : a set of page number that MUST be printed BEFORE key
   - (second val) : a set of page number that MUST be printed AFTER key
   "
  [s]
  (->> (s/split-lines s)
       (map #(re-matches #"(\d+)\|(\d+)" %))
       (remove nil?)
       (map (fn [[_s before after]]
              (vector (Integer/parseInt before)
                      (Integer/parseInt after))))
       (reduce (fn [res [n m]]
                 (-> res
                     (update n (fnil update-rule-after  empty-rule) m)
                     (update m (fnil update-rule-before empty-rule) n))) {})))

(comment
  (create-rules-map sample-input)
  ;;
  )

(defn parse-page-updates
  "Given puzzle input, returns a vector of int representing the page number to update
   in the input order.
   "
  [s]
  (->> (s/split-lines s)
       (map #(s/split % #","))
       (remove #(= 1 (count %)))
       (mapv #(mapv (fn [p] (Integer/parseInt p)) %))))

(comment
  (parse-page-updates sample-input)
  ;;
  )

(defn create-page-update-partitions
  "For a given page update, returns a vector of splitted page update,
   returns a list of partitions where :
   
   - `second` : is a singleton that contains the pivot page number
   - `first`  : is the set of page numbers to print BEFORE pivot page
   - `last`  : is the set of page numbers to print AFTER pivot page

   "
  [page-update]
  (reduce (fn [res v]
            (let [[start middle end] (partition-by #(= % v) page-update)
                  padded-parts       (cond
                                       (and (nil? end)
                                            (= 1 (count start)))
                                       [#{-1} start middle]

                                       (and (nil? end)
                                            (= 1 (count middle)))
                                       [start middle #{-1}]

                                       :else
                                       [start middle end])
                  padded-sets        (map set padded-parts)]
              (conj res padded-sets))) [] page-update))

(comment
  (create-page-update-partitions [1 2 3 4])
  ;;
  )
  ;; so, for each list returned we must check that 
  ;; - pn = (second part)
  ;; - update_before = (first part)
  ;; - ruleAfter = (second (get rules pn))
  ;; - intersec update_before, ruleAfter is empty

(defn validate-partition [[before-set pivot-set after-set] rules-map]
  (let [pivot-pn                 (first pivot-set)
        [rule-before rule-after] (get rules-map pivot-pn)]
    (and (empty? (intersection before-set rule-after))
         (empty? (intersection after-set  rule-before)))))

(comment
  (validate-partition '(#{23} #{55} #{65 32 13 34 11}) {55 [#{1 2} #{3 4}]})
  (validate-partition '(#{23} #{55} #{65 32 13 34 11}) {55 [#{1 2} #{}]})
  (validate-partition '(#{23} #{55} #{65 32 13 34 11}) {55 [#{} #{2 4}]})
  (validate-partition '(#{23} #{55} #{65 32 13 34 11}) {55 [#{65} #{2 4}]})
  (validate-partition '(#{23} #{55} #{65 32 13 34 11}) {55 [#{9} #{65 999 23}]})
  (validate-partition '(#{23} #{55} #{65 32 99 34 11}) {55 [#{9} #{65}]})
  (validate-partition '(#{23} #{55} #{65 32 99 34 11}) {55 [#{9} #{65 102}]})

  ;;
  )

(defn invalid-order [rules-map page-update]
  (->> (create-page-update-partitions page-update)
       (every? #(validate-partition % rules-map))))

(comment
  (invalid-order {32 [#{25} #{51 55}]} [32 51 77])
  (invalid-order {32 [#{25} #{51 55}]} [32 51 25  77])
  (invalid-order {32 [#{25} #{51 55 70}]} [32 51 77])
  (invalid-order {32 [#{25 2} #{51 55 70}]} [32 51 77])

  ;;
  )

(defn solution-1 []
  (let [puzzle-input   (slurp "resources/day_5.txt")
        rules-map      (create-rules-map puzzle-input)
        invalid-order? (partial invalid-order rules-map)]
    (->> (parse-page-updates puzzle-input)
         (filter invalid-order?)
         (map #(nth % (quot (count %) 2)))
         (apply +))))

(println (format "solution 1 = %d" (solution-1)))
;; => 7307 ⭐


;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For each of the incorrectly-ordered updates, 
;; use the page ordering rules to put the page numbers in the right order

;; finding the incorrectly oredered updates can be done <ith part of solution 1

(defn incorrectly-ordered-updates [rules-map page-updates]
  (let [invalid-order? (partial invalid-order rules-map)]
    (remove invalid-order? page-updates)))

(comment
  (def puzzle-input (slurp "resources/day_5.txt"))
  (def puzzle-input sample-input)
  (incorrectly-ordered-updates (create-rules-map puzzle-input) (parse-page-updates puzzle-input))
;;
  )

;; but then ...
;; .... we must find which page numbers are breaking rule and switch their position to
;; get a correctly-ordered updates.

(defn pairs-to-test
  "Given a list of page updates, returns a list of maps for each page number pair where
  
  - `:index` : vector [index-n index-m]
  - `:pn` : vector [n m]

  Example:
  ```clojure
  (pairs-to-test  [1 2 3])
  =>  (
        {:index [0 1], :pn [1 2]} 
        {:index [0 2], :pn [1 3]} 
        {:index [1 2], :pn [2 3]}
      )
  ```
  "
  [pg-update-xs]
  (for [x (range 0 (count pg-update-xs))
        y (range (inc x) (count pg-update-xs))]
    {:index [x y]
     :pn    [(nth pg-update-xs x) (nth pg-update-xs y)]}))

(defn unordered-pair
  "Returns TRUE if page update pair `[n m]` complies with ordering rules provided by `rules-map`"
  [rules-map [n m]]
  (boolean (let [[before-set _after-set] (get rules-map n)
                 [_before-set after-set] (get rules-map m)]
             (or (and before-set
                      (before-set m))
                 (and after-set
                      (after-set n))))))
(comment

  (unordered-pair {2 [#{1} #{2}]} [2 3])
  (unordered-pair {2 [#{1} #{2}]} [22 35])
  (unordered-pair {2 [#{3} #{6}]} [2 3])
  (unordered-pair {2 [#{1} #{3}]} [2 3])
  (unordered-pair {2 [#{1} #{3}]
                   3 [#{1} #{2}]} [2 3])
  ;;
  )


(defn find-unordered-pairs
  "Returns all page updates in `page-update-xs` with incorrect order for the given `rules-map`"
  [rules-map page-update-xs]
  (->> (pairs-to-test page-update-xs)
       (filter #(unordered-pair rules-map (get % :pn)))))

(comment
  (find-unordered-pairs {2 [#{1} #{3}]} [1 2 3])
  (find-unordered-pairs {3 [#{1} #{2}]} [1 2 3])
  ;;
  )


(defn swap
  [items i j]
  (assoc items i (items j) j (items i)))

(defn reorder-page-updates [rules-m page-update]
  (loop [pg-update-xs         page-update
         first-unordered-pair (first (find-unordered-pairs rules-m page-update))]
    (if-not first-unordered-pair
      pg-update-xs
      (let [swapped-updates (swap pg-update-xs
                                  (first  (:index first-unordered-pair))
                                  (second (:index first-unordered-pair)))]
        (recur swapped-updates (first (find-unordered-pairs rules-m swapped-updates)))))))

(comment
  (reorder-page-updates {2 [#{3} #{5}]} [1 2 3])
  (reorder-page-updates {2 [#{3} #{5}]
                         4 [#{1 2} #{6 7 9}]} [1 2 3 6 4 10])
  ;;
  )


(defn solution-2 []
  (let [puzzle-input (slurp "resources/day_5.txt")
        page-updates (parse-page-updates puzzle-input)
        rules-map    (create-rules-map puzzle-input)]

    (->> (incorrectly-ordered-updates rules-map page-updates)
         (map #(reorder-page-updates rules-map %))
         (map #(nth % (quot (count %) 2)))
         (apply +))))

(println (format "solution 2 = %d" (solution-2)))
;; :=> 4712 ⭐
