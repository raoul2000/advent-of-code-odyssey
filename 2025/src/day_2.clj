(ns day-2
  (:require [clojure.string :as s]))


;; https://adventofcode.com/2025/day/2

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Find invalid Ids
;; The ranges are separated by commas (,); each range gives its first ID and last ID separated by a dash (-).

;; You can find the invalid IDs by looking for any ID which is made 
;; only of some sequence of digits repeated twice. So, 55 (5 twice), 6464 (64 twice), and 123123 (123 twice) would all be invalid IDs.
;; None of the numbers have leading zeroes; 0101 isn't an ID at all. (101 is a valid ID that you would ignore.)

(def sample-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(def puzzle-input (slurp "resources/day_2.txt"))

(comment
  ;; this could be solved using regex  where the secong group must
  ;; be equal to the first group

  (re-matches #"^(\d+)(\1)$" "11")
  (re-matches #"^(\d+)(\1)$" "1188511885")
  (re-matches #"^(\d+)(\1)$" "1188511886")

  ;
  )

(defn solution-1 [input]
  (->> (apply concat (->> (s/split input #",")
                          ;; pairs of string
                          (map #(s/split % #"-"))
                          ;; pairs of integers
                          #_(map #(vector (Integer/parseInt (first %)) (Integer/parseInt (second %))))
                          (map #(vector (bigint (first %)) (bigint (second %))))
                          ;; to ranges
                          (map #(conj (vec (apply range %)) (second %)))))
       ;; convert to string
       (map #(format "%s" %))
       ;; remove string with odd length (cannot match)
       (filter #(even? (count %)))
       ;; keep patterns 
       (filter #(re-matches #"^(\d+)(\1)$" %))
       ;; string -> int
       (map #(bigint %))
       ;; sum it all
       (reduce +)))

(comment
  (solution-1 sample-input)
  ;; => 1227775554  looking good
  ;;

  (solution-1 puzzle-input)
  ;; => 40214376723 ...⭐
  )


;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now, an ID is invalid if it is made only of some sequence of digits repeated at least twice. 
;; So, 12341234 (1234 two times), 123123123 (123 three times), 1212121212 (12 five times), and 1111111 (1 seven times) 
;; are all invalid IDs.

(defn solution-2 [input]
  (->> (apply concat (->> (s/split input #",")
                          ;; pairs of string
                          (map #(s/split % #"-"))
                          ;; pairs of integers
                          #_(map #(vector (Integer/parseInt (first %)) (Integer/parseInt (second %))))
                          (map #(vector (bigint (first %)) (bigint (second %))))
                          ;; to ranges
                          (map #(conj (vec (apply range %)) (second %)))))
       ;; convert to string
       (map #(format "%s" %))
       ;; keep patterns 
       (filter #(re-matches #"^(\d+)((\1))+$" %))
       ;; string -> int
       (map #(bigint %))
       ;; sum it all
       (reduce +)))


(comment
  
  (solution-2 sample-input)
  ;; => 4174379265 ... okay
  
  (solution-2 puzzle-input)
  ;; => 50793864718 ... ⭐⭐ 

  ;;
  )