(ns day-9
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2024/day/9

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - disk map
;; - blocks

(def sample-input "2333133121414131402")

(def puzzle-input (slurp "resources/day_9.txt"))

(comment

  (def blocks (:blocks (reduce-kv (fn [res index c]
                                    (let [block-length  (- (int c) 48)
                                          id-number     (:cur-id res)]
                                      (if (even? index)
                       ;; dealing with file length
                                        (-> res
                                            (update :cur-id inc)
                                            (update :blocks into (repeat block-length id-number)))
                       ;; dealing with free space
                                        (-> res
                                            (update :blocks into (repeat  block-length -1))))))
                                  {:cur-id 0
                                   :blocks []}
                                  (vec puzzle-input))))


  ;; add index
  (def indexed-blocks (map-indexed vector blocks))

  (def empty-space? #(= -1 (second %)))
  ;; blocks to move
  (def moveable-blocks (->> indexed-blocks
                            (remove empty-space?)
                            reverse))

  (def block-index first)

  (def result (loop [blocks          indexed-blocks
                     moveable-blocks moveable-blocks
                     final           []]
                (if (or (empty? moveable-blocks)
                        (< (block-index (first moveable-blocks))
                           (block-index (first blocks))))
                  final
                  (recur (rest blocks)
                         (if (empty-space? (first blocks))
                           (rest moveable-blocks)
                           moveable-blocks)
                         (if (empty-space? (first blocks))
                           (conj final (first moveable-blocks))
                           (conj final (first blocks)))))))

  (->> result
       (map-indexed (fn [idx v] (vector idx (second v))))
       (map #(apply * %))
       (reduce +))

  ;; => 6446899523367 ⭐


  ;;
  )
