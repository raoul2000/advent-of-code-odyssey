(ns day-10
  (:require [clojure.zip :as z]
            [clojure.string :as s]))

;; https://adventofcode.com/2024/day/10

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - map indicate 'node-height'
;; - find all 'trails'
;; - a trail starts at trailhead position (node-height = 0)
;; - trail is any path that starts at node-height 0, ends at node-height 9, 
;;   and always increases by a node-height of exactly 1 at each step
;; - only up, down, left, or right

(def sample-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(def puzzle-input (slurp "resources/day_10.txt"))

;; we need grid related functions (from other puzzles)

(defn create-grid [s]
  (->> (s/split-lines s)
       (mapv #(mapv (fn [c] (Character/digit c 10))  (vec %)))))

(comment
  (create-grid sample-input)
  ;;
  )

(defn height-at [grid [x y]]
  (get-in grid [y x]))

(defn grid-size
  "Returns [col-count line-count] for the given grid"
  [grid]
  [(count (first grid)) (count grid)])

(defn in-grid?
  "Return TRUE if pos [x y] is inside the given `grid`"
  [grid [x y]]
  (let [[col-count line-count] (grid-size grid)]
    (and (< -1 x col-count)
         (< -1 y line-count))))

(defn trailhead-pos
  "Given a `grid` return all `x y` position with a node-height equal to zéro."
  [grid]
  (let [[col-count line-count] (grid-size grid)]
    (for [x (range 0 col-count)
          y (range 0 line-count)
          :when (zero? (height-at grid [x y]))]
      [x y])))

(comment
  (trailhead-pos (create-grid sample-input))

  ;;
  )
;; a node is a map describing a portion of a trail. It has following keys
;; - step : a 3-uplet describing one position of the grid as [ height x y ] 
;; - content : a vector of nodes describing each steps that can be reached from the current node

(defn node-pos-xy
  "Given a `node` returns the corresponding [x y] position in the grid"
  [node]
  (-> node :step rest vec))

(defn node-height
  "Given a `node` returns the corresponding height "
  [node]
  (-> node :step first))

(comment
  (node-pos-xy {:step [3 5 6]})
  ;; => []5 6]
  (node-height {:step [3 5 6]})
  ;; => 3
  ;;
  )


(defn find-next-steps
  "Given a grid and a step, find all next steps. Return empty seq when none was found."
  [grid [h x y :as _step]]
  (let [next-height  (inc h)]
    (->> [[(dec x) y]
          [(inc x) y]
          [x      (dec y)]
          [x      (inc y)]]
         (filter #(and (in-grid? grid %)
                       (= next-height (height-at grid %))))
         (map #(into [next-height] %)))))

(comment
  (def grid (create-grid sample-input))

  (find-next-steps grid [0 0 6])
  ;; => ([1 1 6] [1 0 7])
  (find-next-steps grid [0 2 0])
    ;; => ([1 3 0] [1 2 1])
  (find-next-steps grid [1 0 7])
  ;; => ()

  ;;
  )

;; a node is an item stored in an xml-zipper

(defn step->node
  "Given a step provided as a triplet [height x y ] returns a map describing the corresponding step"
  [step]
  {:step step})

(defn node->step
  "Given a `node`returns the triplet step vector"
  [node]
  (:step node))

(defn trailhead-nodes
  "Given a grid, returns a seq of nodes corresponding to trail head steps"
  [grid]
  (->> (trailhead-pos grid)
       (map #(into [0] %))
       (map step->node)))

(comment
  (trailhead-nodes (create-grid sample-input))
;;
  )

(defn create-root-node
  "Given a grid, returns a map describing the zip root node where all root node children
    are trailhead nodes."
  [grid]
  {:step [-1 -1 -1]
   :content (trailhead-nodes grid)})

(comment
  (create-root-node (create-grid sample-input))
;;
  )

(comment
  (def grid (create-grid sample-input))
  (def trail-zip (z/xml-zip (create-root-node grid)))

  ;; traverse depth first the zipper

  (->>  (iterate z/next trail-zip)
        (take-while (complement z/end?))
        last
        z/node)

  ;; explore step by step and append child
  (-> trail-zip
      z/next
      (z/append-child (step->node [2 2 2]))
      z/next
      (z/append-child (step->node [3 33 4]))
      z/next
      z/node)
      ;;
  )

(defn add-next-steps
  "Given a `grid` and a `loc` describing the current position in the grid, add
  all next steps that can be reached from this loc as children of  `loc` and returns
  it."
  [grid loc]
  (if (z/end? loc)
    loc
    (->>
     ;; given the current loc
     (z/node loc)
     ;; get step info for this loc
     node->step
     ;; get all next steps
     (find-next-steps grid)
     ;; convert to nodes
     (map step->node)
     ;; append nodes as children of current node
     (reduce z/append-child loc))))

(defn find-all-trails
  "Given a `grid` returns a loc describing all trails. Each node is a step with next
   step set as children nodes.

   Note that the root node is not an actual step, it exists only as the parent of trail heads.
  "
  [grid]
  (->> grid
       create-root-node
       z/xml-zip
       (iterate (fn [this-loc]
                  (->> (add-next-steps grid this-loc)
                       z/next)))
       (take-while (complement z/end?))
       last
       z/root
       z/xml-zip))

(defn build-path
  "Given a `loc` returns a seq of steps up to the trail head step."
  [loc]
  (rest (conj (mapv :step (z/path loc)) (node->step (z/node loc)))))

(defn solution-1 [input]
  (->> (create-grid input)
       find-all-trails
       ;; navigate depth first
       (iterate z/next)
       ;; .. until all nodes have been visited
       (take-while (complement z/end?))
       ;; only keep locs of lead nodes (height = 9)
       (filter #(= 9 (node-height (z/node %))))
       ;; create path to trail head
       (map build-path)
       ;; create map where k is trail head and val is a seq
       ;; of all steps
       (group-by first)
       ;; for each trails, ensure no trail tail duplicate
       (map (fn [[k v]]
              [k (reduce (fn [acc i]
                           (conj acc (last i))) #{} v)]))
       ;; count trail tails
       (map (fn [[k v]]
              [k (count v)]))
       (map last)
       (reduce +)))

(comment
  (solution-1 sample-input)
  ;; => 36 good

  (solution-1 puzzle-input)
  ;; => 593 ⭐
  ;;
  )
