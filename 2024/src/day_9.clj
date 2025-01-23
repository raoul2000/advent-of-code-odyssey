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

  (def blocks (build-blocks sample-input))
  (def blocks (build-blocks puzzle-input))
  ;; => [0 0 -1 -1 -1 1 1 1 -1 -1 -1 2 -1 -1 -1 3 3 3 -1 4 4 -1 5 5 5 5 -1 6 6 6 6 -1 7 7 7 -1 8 8 8 8 9 9]


  ;; to parittioned blocks
  (partition-by identity blocks)

  ;; to partitionned block 2
  (defn build-block-descriptors [blocks]
    (->> blocks
         (partition-by identity)
         (map (juxt first count))
         (reduce (fn [res [type size]]
                   (let [prev-pos  (:prev-pos res)
                         prev-size (:prev-size res)
                         new-pos   (if prev-pos (+ prev-pos prev-size) 0)]
                     (-> res
                         (assoc   :prev-size size)
                         (assoc   :prev-pos new-pos)
                         (update  :blocks conj [new-pos type size false])))) {:prev-pos  nil
                                                                              :prev-size nil
                                                                              :blocks    []})
         :blocks))

  (build-block-descriptors blocks)

  ;; block model : [index type size]
  (def block-index first)
  (def block-type second)
  (def block-size (comp last butlast))
  (def block-moved? last)

  (block-size [22 5 22 false])

  (defn free-space?
    "Returns TRUE if the block is a freespace block"
    [block]
    (= -1 (block-type block)))

  (free-space? [1 1 2 false])
  (free-space? [1 -1 2 false])

  (defn can-hold?
    "Returns TRUE if the `target-block` is a freespace with enough space
    to receive `file-block`"
    [target-block file-block]
    (and (free-space? target-block)
         (>= (block-size target-block)
             (block-size file-block))))

  (can-hold? [0 -1 2 true] [11 5 2 true])
  (can-hold? [0 -1 2 true] [11 5 3 true])
  (can-hold? [0 -1 3 true] [11 5 2 true])
  (can-hold? [0 1 3 true] [11 5 2 true])

  (defn expand-block
    "Returns the disk map for the given block"
    [block]
    (repeat (block-size block) (block-type block)))

  (expand-block [1 5 2 true])
  (expand-block [8 5 4 true])
  (expand-block [8 -1 4 true])

  (defn pad [n coll val]
    (take n (concat coll (repeat val))))

  ;; TODO: change this fn after have added fourth item to block descriptor (moved?)
  (defn merge-blocks [empty-block file-block]
    (if (can-hold? empty-block file-block)
      (map #(into [] %) (partition-by identity (pad (count (expand-block empty-block)) (expand-block file-block) -1)))
      (throw (ex-info "empty block too small or not empty" {:empty-block empty-block
                                                            :file-block file-block}))))

  (merge-blocks [0 -1 3 true] [11 2 2 true])
  ;; => ([2 2] [-1])
  (merge-blocks [0 -1 3 true] [11 2 3 true])
  ;; => ([2 2 2])
  (merge-blocks [0 -1 3] [11 2 4])
  ;; => ([2 2 2])


  ;; to block seq
  (into [] (flatten (partition-by identity blocks)))

  ;; merge indexed blocks
  ;; [2 -1 3] +  [40 9 3] => [2 9 3]   with no remainder 
  ;; [2 -1 3] +  [40 9 2] => [2 9 2] [4 -1 1] with remainder

  (defn merge-blocks-2 [[e-idx e-id e-size :as empty-block] [_ f-id f-size :as file-block]]
    (cond
      (= e-size f-size)   [[e-idx f-id f-size]]
      (> e-size f-size)   [[e-idx f-id f-size]
                           [(+ e-idx f-size) e-id (- e-size f-size)]]
      :else  (throw (ex-info "empty block too small or not empty" {:empty-block empty-block
                                                                   :file-block file-block}))))

  (merge-blocks-2 [2 -1 3] [40 9 3])
  (merge-blocks-2 [2 -1 3] [40 9 2])

  (defn move-block [block-xs block-to-move]
    (reduce (fn [res cur-block]
              (let [placed? (:placed res)]
                (if placed?
                  ;; moveable block already placed
                  (if (not= cur-block block-to-move)
                    (update res :output conj cur-block)
                    (update res :output conj [(block-index cur-block) -1 (block-size cur-block)]))

                  (if (and (< (block-index cur-block) (block-index block-to-move))
                           (can-hold? cur-block block-to-move))
                    (-> res
                        (update :output into (merge-blocks-2 cur-block block-to-move))
                        (assoc  :placed true)
                        (assoc :cur-block cur-block))
                    (update res :output conj cur-block)))))
            {:placed false
             :output []}
            block-xs))

  (move-block (build-block-descriptors blocks) [40 9 2])
  (def block-xs (build-block-descriptors blocks))



  (->> (reduce (fn [res block-to-move]
                 (:output (move-block res block-to-move)))
               block-xs
               (reverse block-xs))
       (map expand-block)
       flatten
       (into [])
       (map #(if (= -1 %) 0 %))
       (map-indexed #(* %1 %2))
       (reduce +))

  ;; => 6487236576956 :()









  ;;
  )
