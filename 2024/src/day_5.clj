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

(solution-1)
;; => 7307 ⭐


;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


