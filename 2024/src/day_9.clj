(ns day-9
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2024/day/9

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - disk map
;; - blocks

(def sample-input "2333133121414131402")

(def puzzle-input (slurp "resources/day_9.txt"))

;; convert disk-map to blocks
(comment

  (map (fn [n] (- (int n)  48)) sample-input)
  (apply str (flatten (map-indexed (fn [idx c]
                                     (repeat (- (int c)  48) (if (odd? idx) \. \x))) sample-input)))


  (def blocks-m (reduce-kv (fn [res index c]
                             (let [block-length  (- (int c) 48)
                                   id-number     (:cur-id res)]
                               (if (even? index)
                   ;; dealing with file length
                                 (-> res
                                     (update :cur-id inc)
                                     (update :blocks conj [id-number block-length]))
                   ;; dealing with free space
                                 (-> res
                                     (update :blocks conj [-1 block-length])))))
                           {:cur-id 0
                            :blocks []}
                           (vec sample-input)))

;; [[type length]]
;; - type = -1 : empty space
;; - else file-id


  (defn free-space-length [block-xs]
    (->> block-xs
         (filter #(= -1 (first %)))
         (map second)
         (apply +)))

  (free-space-length (:blocks blocks-m))

  (defn expand-file-blocks [block-xs]
    (->> block-xs
         (remove #(= -1 (first %)))
         (map #(repeat (second %) (first %)))
         flatten))

  (expand-file-blocks (:blocks blocks-m))




  ;;
  )