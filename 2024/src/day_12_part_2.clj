(ns day-12-part-2
  (:require [clojure.string :as s]))


;; https://adventofcode.com/2024/day/12

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

;; After the failure of the first attempt that was based on data model from
;; solution-1, let's try again

(def sample-input-1 "AAAA
BBCD
BBCC
EEEC
")
(def sample-input-2 "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
")

(def sample-input-3 "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
")

(def puzzle-input (slurp "resources/day_12.txt"))

;; let's keep some function from part 1

(defn create-garden [s]
  (->> (s/split-lines s)
       (mapv #(mapv identity  (vec %)))))

(defn print-garden
  "Print given `garden` as string to stdout."
  [garden]
  (->> garden
       (map (partial apply str))
       (s/join "\n")
       print))

(defn plant-at [garden [x y]]
  (get-in garden [y x]))

(defn garden-size
  "Returns [col-count line-count] for the given garden"
  [garden]
  [(count (first garden)) (count garden)])

(defn in-garden?
  "Return TRUE if pos [x y] is inside the given `garden`"
  [garden [x y]]
  (let [[col-count line-count] (garden-size garden)]
    (and (< -1 x col-count)
         (< -1 y line-count))))

(defn set-plant [garden [x y] c]
  (assoc-in garden [y x] c))

(defn update-garden
  "Returns the given `garden` where each position in the `pos-xs` is marked with character `c`."
  [garden pos-xs c]
  (reduce (fn [acc pos]
            (set-plant acc pos c))
          garden
          pos-xs))

(comment
  (print-garden (update-garden (create-garden sample-input-1)  [[0 0] [1 1] [2 3]] \.))
  ;;
  )

(defn all-pos
  "Returns a seq of all pos in the given `garden`, from top-left to bottom-right."
  [garden]
  (let [[col-count line-count] (garden-size garden)]
    (for [x (range 0 col-count)
          y (range 0 line-count)]
      [x y])))

;; function to find regions can also be re-used

(defn touching-pos-xs [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn find-adjacent-pos
  "Given a `[x y ]` pos, returns all adjacent pos with the same plant than the one at `[x y]`."
  [garden [x y]]
  (->> (touching-pos-xs [x y])
       (filter #(in-garden? garden %))
       (filter #(= (plant-at garden [x y]) (plant-at garden %)))))

(defn region-at-pos
  "Returns a region from a `garden` that includes position `[x y]`."
  [garden [x y]]
  (loop [region       #{[x y]}
         candidate-xs (find-adjacent-pos garden [x y])]
    (if (empty? candidate-xs)
      region
      (recur
       (into region candidate-xs)
       (->> candidate-xs
            (mapcat (partial find-adjacent-pos garden))
            (remove region)
            set)))))

(defn already-visited? [garden pos]
  (= \. (plant-at garden pos)))

(defn mark-region-visited [garden region]
  (update-garden garden region \.))

(defn find-all-regions
  "Given a `initial-garden` returns a seq of all regions in this garden."
  [initial-garden]
  (:regions (reduce (fn [{:keys [garden] :as state} pos]
                      (if (already-visited? garden pos)
                        state
                        (let [new-regions (region-at-pos garden pos)]
                          (-> state
                              (assoc  :garden  (mark-region-visited garden new-regions))
                              (update :regions conj new-regions)))))
                    {:garden initial-garden
                     :regions []}
                    (all-pos initial-garden))))

;; now, to represent a fence we are not going to use the [x y] pos of the fence (that could
;; overlap on thin inner spaces) but a triplet [x y DIR] 
;; - x , y : the pos of the plant int he region
;; - DIR : one of :up :down :left :right the direction pointing to the fence relatively to the plant
;;
;; for example : 
;; [0 0 :left]  => fence pos [-1 0]
;; [0 0 :right] => fence pos [1  0]
;; [0 0 :up]    => fence pos [0 -1]
;; [0 0 :down]  => fence pos [0  1]
;;
;; If in a fence list we have : [0 1 :up] [1 1 :up] [2 1 :up]
;; ..then we have a side, horizontal, made of 3 fences
;; If we have [0 1 :down] [1 1 :down] [2 1 :down]
;; ... then we have a side, horizontal too, made of 3 fences
;; If we have : [1 0 :left] [1 1 :left]
;; ... then we have a side, vertical made of 2 fences

;; Ok. Given a region, let's find all fences

(def coords butlast)
(def x-coord first)
(def y-coord second)
(def direction last)

(defn touching-pos-with-dir-xs [[x y]]
  [[(inc x) y :right] [(dec x) y :left] [x (inc y) :down] [x (dec y) :up]])

(defn find-fences
  "Find and returns a set of fences for the given `region` in the given `garden`"
  [garden region]
  (let [plant (plant-at garden (first region))]
    (->> region
         (map (juxt identity touching-pos-with-dir-xs))
         (map (fn [[reg-pos adjacents]]
                (filter  (fn [adj]
                           (complement (region (coords adj)))
                           ) adjacents)
                ))
         )))

(comment

  (find-fences (create-garden sample-input-1)
               #{[0 0] [1 0] [3 0] [2 0]})
  (touching-pos-xs [0 1])
  ;;
  )
