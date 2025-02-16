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
       #_(take-while (complement z/end?))
       (take 12)
       last
       z/root
       z/xml-zip))


(comment

  (->> (find-all-trails (create-grid sample-input))
       (iterate z/next)
       (take-while (complement z/end?))
       (filter #(= 9 (node-height (z/node %))))
       (map z/path)
       #_(map z/node))



  (def trails (let [grid (create-grid sample-input)
                    loc (z/xml-zip (create-root-node grid))]

                (->> (iterate (fn [l]
                                (->> (add-next-steps grid l)
                                     z/next))  loc)
                     #_(take-while (complement z/end?))
                     (take 12)
                     last
                     z/root
                     z/xml-zip)))


  (-> trails
      z/next
      z/right
      z/node)
  (->> (iterate z/next trails)
       (take 11)
       last
       z/node
       node-height)

  ;; browse a loc and return all trails
  (->> (iterate z/next trails)
       (reduce (fn [acc, loc]
                 (if (z/end? loc)
                   (reduced acc)
                   (if (not= 9 (node-height (z/node loc)))
                     acc
                     (conj acc (z/path loc))))) [])
       #_(map :step))

 ;; how to build path to root ?

  (map :step (-> {:step [-1 -1 -1]
                  :content [{:step [1 1 1] :content [{:step [2 3 3]
                                                      :content [{:step [3 4 4]}]}]}
                            {:step [1 2 2]}]}
                 z/xml-zip
                 z/next
                 z/next
                 z/next
                 #_(map (fn [end-node]
                          (into [end-node] (->> (z/path end-node)
                                                (map z/node)
                                                (map :step)))))
                 #_z/node
                 z/path))

  (defn build-path [loc]
    (conj (mapv :step (z/path loc)) (node->step (z/node loc))))

  (-> {:step [-1 -1 -1]
       :content [{:step [1 1 1] :content [{:step [2 3 3]
                                           :content [{:step [3 4 4]}]}]}
                 {:step [1 2 2]}]}
      z/xml-zip
      z/next
      z/next
      z/next
      #_(map (fn [end-node]
               (into [end-node] (->> (z/path end-node)
                                     (map :step)))))
      #_z/node
      #_z/path
      build-path)
;;
  )

(defn explore-trails
  ""
  [grid loc]
  (if (z/end? loc)
    loc
    (let [next-loc    (z/next loc)
          next-steps  (find-next-steps grid (node->step (z/node next-loc)))]
      (reduce (fn [res step]
                (z/append-child res (step->node step))) next-loc next-steps))))





(comment
  (->> (take-while (complement z/end?) (iterate (partial explore-trails grid) trail-zip))
       last
       z/root)


  (defn count-full-trails
    "Given a `loc` return the count of nodes having the same height as `max-height`"
    [loc max-height]
    (loop [loc loc
           pos-final #{}]
      (if (z/end? loc)
        (count pos-final)
        (recur (z/next loc)
               (if (= max-height (node-height  (z/node loc)))
                 (conj pos-final (z/node loc))
                 pos-final)))))

  (count-full-trails (z/xml-zip {:step [0 0 6],
                                 :content
                                 [{:step [3 8 8] :content [{:step [9 8 8]}
                                                           {:step [9 8 8]}
                                                           {:step [9 8 8] :content [{:step [1 2 3]}
                                                                                    {:step [9 2 3]}]}]}]}) 9)

  (def zp-006 (z/xml-zip {:step [-1 -1 -1] :content [{:step [0 0 6]}]}))
  ;; => 5 trails
  (def zp-006 (z/xml-zip {:step [-1 -1 -1] :content [{:step [0 1 7]}]}))
  ;; => 5 trails
  (def zp-006 (z/xml-zip {:step [-1 -1 -1] :content [{:step [0 2 0]}]}))

  (def zp-006-explore (->> (take-while (complement z/end?) (iterate (partial explore-trails grid) zp-006))
                           last
                           z/root
                           z/xml-zip))

  (count-full-trails zp-006-explore 9)


  (->> (take-while (complement z/end?)  (iterate (partial explore-trails grid) trail-zip))
       last
       z/root
         ;; get all trailhead nodes
       :content
         ;; create xml zipper for each trailhead node
       (map z/xml-zip)
       (map #(count-full-trails % 9)))


  ;;
  )



;; try using a zip

(comment
  ;; trailhead
  {:pos [2 0] :next []}

  ;; second step
  {:pos [2 0] :next [{:pos [3 0] :next []}
                     {:pos [2 1] :next []}]}

  ;; last (leaf)
  {:pos [2 0] :next [{:pos [3 0]}
                     {:pos [2 1]}]}

  ;; other model
  ;; trailhead
  [[2 0] []]

  ;; second step
  [[2 0] [[[3 0] []]
          [[2 1] []]]]

  (defn branch? [node]
    (and   (= 2 (count node))
           (vector? (second node))))
  (def children second)

  (defn make-node [node children]
    #_[(vec (first node)) (into (second node) children)]
    [(vec (first node)) (vec children)])

  [[1 2]]

  (z/zipper (constantly true)
            rest
            (fn [_ c] c) [1])

  (make-node [[:a :b] []]
             [[[:1 :2] []]
              [[:4 :5] []]])

  (def zp (z/vector-zip [1]))
  (z/vector-zip '[5 [10 20 30] [1 2 3]])
  (def zp (z/zipper branch? children make-node [[-1 -1] []]))

  (z/insert-child zp [9 9])
  zp

  ([[:1 :1] [[[:ch :ch] []] [[:ch2 :ch2] []]]]
   [[-1 -1] [[[:3 :3] []] [[:2 :2] []] [[:1 :1] []]]])


;; The above can also be written like this using the thread macro style
  (-> {:tag :root
       :pos [2 3]
       :content [{:tag :any
                  :pos [6 7] :content [[:tag :any]]}]}
      z/xml-zip
      z/down
      z/node)

  (-> {:pos [1 2]
       :content [{:pos [2 3]}
                 {:pos [2 5]
                  :content [{:pos [3 7]}]}]}
      z/xml-zip
      z/next
      z/next
      z/next
      #_(z/append-child {:pos [8 8]})
      #_(z/append-child {:pos [0 0] :content [[:pos [11 22]]]})
      z/next
      z/node)

  (def loc {:pos false
            :content [{:pos [1 2]}
                      {:pos [4 2]
                       :content [{:pos [999 888]}]}
                      {:pos [6 8]}]})
  (-> (z/xml-zip loc)
      z/down
      z/branch?)

  (->> (z/xml-zip loc)
       (iterate z/next)
       (take-while (complement z/end?))
       (filter   #(zero? (count (z/children %))))
       (map (comp :pos z/node)))


  ;;
  )

