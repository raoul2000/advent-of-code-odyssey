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
;; - Attempt to move each file exactly once in order of decreasing file ID number starting with the file with the 
;;   highest file ID number

;; data model : each block is represented by a 4 items vectors

(defn build-block-descriptors
  "Given a seq of numbers describing each disk position, returns a seq of vectors where each vector
   describes a block :
   ```
   [ index type size moved?]
   ```
   "
  [blocks]
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

(comment
  (build-block-descriptors (build-blocks sample-input))
  ;;
  )

(def block-type second)
(def block-size (comp last butlast))
(def block-moved? last)
(defn free-space?
  "Returns TRUE if the block is a freespace block"
  [block]
  (= -1 (block-type block)))

(defn can-hold?
  "Returns TRUE if the `target-block` is a freespace with enough space
    to receive `file-block`"
  [target-block file-block]
  (and (free-space? target-block)
       (>= (block-size target-block)
           (block-size file-block))))

(defn expand-block
  "Returns the disk map for the given block"
  [block]
  (repeat (block-size block) (block-type block)))

(defn merge-blocks-2 [[e-idx e-id e-size :as empty-block] [_ f-id f-size :as file-block]]
  (cond
    (= e-size f-size)   [[e-idx f-id f-size true]]
    (> e-size f-size)   [[e-idx f-id f-size true]
                         [(+ e-idx f-size) e-id (- e-size f-size) false]]
    :else  (throw (ex-info "empty block too small or not empty" {:empty-block empty-block
                                                                 :file-block file-block}))))

(defn move-block [block-xs block-to-move]
  (if (or (block-moved? block-to-move)
          (empty-space? block-to-move))
    block-xs
    (->> (reduce (fn [res cur-block]
                   (let [placed? (:placed res)]
                     (if placed?
                                 ;; moveable block already placed
                       (if (not= cur-block block-to-move)
                         (update res :output conj cur-block)
                         (update res :output conj [(block-index cur-block) -1 (block-size cur-block) false]))

                       (if (and (< (block-index cur-block) (block-index block-to-move))
                                (can-hold? cur-block block-to-move))
                         (-> res
                             (update :output into (merge-blocks-2 cur-block block-to-move))
                             (assoc  :placed true)
                             (assoc :cur-block cur-block))
                         (update res :output conj cur-block)))))
                 {:placed false
                  :output []}
                 block-xs)
         :output)))


(defn solution-2 [input]
  (let [block-xs (build-block-descriptors (build-blocks input))]
    (->> (reverse block-xs)
         (reduce move-block block-xs)
         (map expand-block)
         flatten
         (into [])
         (map #(if (= -1 %) 0 %))
         (map-indexed #(* %1 %2))
         (reduce +))))


(comment
  (solution-2 sample-input)
  ;; => 2858 ... good
  (solution-2 puzzle-input)
  ;; => 6478232739671 ⭐
  )
