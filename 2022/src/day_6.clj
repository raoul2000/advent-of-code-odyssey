(ns day-6
  (:require [clojure.string :refer [split-lines split]]
            [clojure.set :refer [intersection]]))

;; https://adventofcode.com/2022/day/6

;; device = communication system
;; the device emits a few colorful sparks
;; the start of a packet is a sequence of four characters that are all different
;; report the number of characters from the beginning of the buffer to the end of the first such four-character marker

(def sample "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def sample-2 "bvwbjplbgvbhsrlpgdmjqwftvncz")
(def sample-3 "nppdvjthqldpwncqszvftbrmjlhg")
(def sample-4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
(def sample-5 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")


(defn solution-1 []
  (reduce (fn [acc [idx c]]
            (if (= 4 (count acc))
              (reduced  [acc idx])
              (if (some #(= c %) acc)
                (conj (into [] (rest (drop-while #(not= c %) acc))) c)
                (conj acc c)))) [] (map-indexed vector (slurp "./resources/puzzle_6.txt"))))

(comment

  (solution-1)
  ;; [[\z \v \p \m] 1238]
  ;; => 1238 !!
  ;;
  )

;; part 2 -------------------------------------------------

;; it also needs to look for messages
;; A start-of-message marker is just like a start-of-packet marker, 
;;    except it consists of 14 distinct characters rather than 4
;;
;;

(defn solution-2 []
  (reduce (fn [acc [idx c]]
            (if (= 14 (count acc))
              (reduced  [acc idx])
              (if (some #(= c %) acc)
                (conj (into [] (rest (drop-while #(not= c %) acc))) c)
                (conj acc c)))) [] (map-indexed vector (slurp "./resources/puzzle_6.txt"))))

(comment
  (solution-2)
  ;; [[\q \s \p \g \b \z \m \j \n \l \r \d \h \v] 3037]
  ;;=> 3037 !!  
  ;;
  )