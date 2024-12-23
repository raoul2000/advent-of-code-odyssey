(ns day-9
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2024/day/9

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - disk map
;; - blocks

(def sample-input "2333133121414131402")

(def puzzle-input (slurp "resources/day_9.txt"))

(defn build-blocks
  "Given `ìnput`, returns a vector of blocks, where each block is a number 'n' : 
    - *n* = -1 : free space block
    - *n* = n : file-id *n* block
  "
  [input]
  (:blocks (reduce-kv (fn [res index c]
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
                      (vec input))))

(comment
  (build-blocks sample-input)
  (build-blocks puzzle-input)
  ;;
  )

(defn build-indexed-blocks
  "Given a vector of blocks, returns a seq of pairs where :
  - first item is the block index
  - second is the block type (-1 or file-id')"
  [block-xs]
  (map-indexed vector block-xs))

(def empty-space? #(= -1 (second %)))
(def block-index first)

(defn build-moveable-blocks [indexed-blocks]
  (->> indexed-blocks
       (remove empty-space?)
       reverse))

(defn defragment-indexed-blocks [indexed-blocks]
  (loop [blocks          indexed-blocks
         moveable-blocks (build-moveable-blocks indexed-blocks)
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

(defn compute-checksum [defragmented-blocks]
  (->> defragmented-blocks
       (map-indexed (fn [idx v] (vector idx (second v))))
       (map #(apply * %))
       (reduce +)))

(defn solution-1 [input]
  (->> input
       build-blocks
       build-indexed-blocks
       defragment-indexed-blocks
       compute-checksum))

(comment

  (solution-1 sample-input)
  ;; => 1928 .. ok 

  (solution-1 puzzle-input)
  ;; => 6446899523367 ⭐

;;
  )
