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

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - move the whole file
;; - by decreasing file id numnber 

(comment

  (def blocks [0 0 -1 -1 -1 1 1 1 -1 -1 -1 2 -1 -1 -1 3 3 3 -1 4 4 -1 5 5 5 5 -1])

  ;; to parittioned blocks
  (partition-by identity blocks)

  ;; to partitionned block 2
  (map-indexed (fn [idx b]
                 (vector idx (first b) (count b))) (partition-by identity blocks))

  (def block-index first)
  (def block-type second)
  (def block-size last)

  (def free-space? (comp (partial = -1) block-type))
  (free-space? [1 1 2])
  (free-space? [1 -1 2])

  (defn can-hold? [target-block file-block]
    (and (free-space? target-block)
         (>= (block-size target-block)
             (block-size file-block))))

  (can-hold? [0 -1 2] [11 5 2])
  (can-hold? [0 -1 2] [11 5 3])
  (can-hold? [0 -1 3] [11 5 2])
  (can-hold? [0 1 3] [11 5 2])

  (defn expand-block [[_idx type size]]
    (repeat size type))

  (expand-block [1 5 2])
  (expand-block [8 5 4])

  (defn merge-blocks [empty-block file-block]
    (map #(into [] %) (partition-by identity (pad (count (expand-block empty-block)) (expand-block file-block) -1))))
  
  (merge-blocks [0 -1 3] [11 2 2])
  ;; => ([2 2] [-1])
  (merge-blocks [0 -1 3] [11 2 3])
  ;; => ([2 2 2])


  ;; to block seq
  (into [] (flatten (partition-by identity blocks)))

  ;; [-1 -1 -1 ] + [ 2 2 ] => [2 2]  [-1]

  (defn pad [n coll val]
    (take n (concat coll (repeat val))))

  (defn merge-blocks [empty-block file-block]
    (map #(into [] %) (partition-by identity (pad (count empty-block) file-block -1))))

  (merge-blocks [-1 -1] [2 2])
  (merge-blocks [-1 -1 -1] [2 2])



  ;;
  )
