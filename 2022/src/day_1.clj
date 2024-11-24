(ns day-1
  (:require [clojure.string :refer [split-lines blank?]]))

;; https://adventofcode.com/2022/day/1

(comment
  (println "let's start !! "))

(defn str-coll->int-coll [coll]
  (map #(Integer/parseInt %) coll))

(defn sum-calories-per-elf []
  (->> (slurp "./resources/puzzle_1.txt")
       split-lines
       (partition-by blank?)
       (remove #(= "" (first %)))
       (map str-coll->int-coll)
       (map #(apply + %))))

(defn find-top-3 []
  (->> (sum-calories-per-elf)
       (sort)
       (reverse)
       (take 3)))

(defn solution []
  (apply + (find-top-3)))

;; => 204837 !!
