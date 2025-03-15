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

;; Wait, maybe something more simple. What if we set coordinates of a fence at - 0.25 or + 0.25 ?
;; For example 
;; [-0.25   0] left vertical fence
;; [ 0.25   0] right vertical fence
;; [ 0   0.25] bottom horizontal fence
;; [ 0  -0.25] top horizontal fence


(def x-coord first)
(def y-coord second)

(defn outside-region? [region [x y]]
  (not (region [x y])))

(defn region-pos->fences [region [x y]]
  (cond-> []
    (outside-region? region [(dec x) y]) (conj  [(- x 0.25) y]) ;; left
    (outside-region? region [(inc x) y]) (conj  [(+ x 0.25) y]) ;; right
    (outside-region? region [x (dec y)]) (conj  [x (- y 0.25)]) ;; up
    (outside-region? region [x (inc y)]) (conj  [x (+ y 0.25)]) ;; bottom
    ))


(defn find-fences
  "Find and returns a set of fences for the given `region`"
  [region]
  (mapcat #(region-pos->fences region %) region))

;; problem here !! I thought we are using vector but the (butlast) returns seq
;; and conj add in first pos

(defn group-by-side [axis-fn fences-by-axis]
  (reduce (fn [acc fence]
            (if (empty? acc)
                ;; the first side
              (conj acc [fence])
                ;; is this fence consecutive to the last fence of the last side ?
              (let [prev-fence (last (last acc))]
                (if (= (inc (axis-fn prev-fence)) (axis-fn fence))
                  ;; consecutive
                  (-> acc
                      butlast
                      (conj (conj (last acc) fence)))
                    ;; not consecutive : start new side
                  (conj acc [fence])))))
          []
          (into [] (sort-by axis-fn fences-by-axis))))

(comment
  (group-by-side y-coord  [[-0.25 6] [-0.25 5] [-0.25 3] [-0.25 2] [-0.25 4]])
  (group-by-side y-coord  [[3.25 4] [3.25 5]])
  (group-by-side y-coord  [[1.25 3] [1.25 5] [1.25 6] [1.25 2]])
  ;; return => ([[1.25 6]] [[1.25 5]] [[1.25 2] [1.25 3]])
  ;; but SHOULD return => ([[1.25 6] [1.25 5]] [[1.25 2] [1.25 3]])

  (group-by-side y-coord  (sort-by second [[1.25 3] [1.25 5] [1.25 6] [1.25 2]]))

  (conj '(-1) 2)
  (into [] (sort [1 3 6 4 2]))
  (butlast [1 2 3])
  ;;
  )

  ;; let's turn above form into a parametrized function
(defn find-sides [region group-fn axis-fn]
  (->> (find-fences region)
       (group-by group-fn)
       (map second)
       ;; keep only fences having the same y-coord
       ;; remove group of one fence : this is not a side
       (remove #(= 1 (count  %)))
       ;; sides are fences with successive x-coord
       (map #(group-by-side axis-fn %))
       ;; aggregate sides parts with a length > 1 : they are sides
       #_(reduce (fn [acc sides-parts]
                 (->> sides-parts
                      (remove #(= 1 (count %)))
                      (into acc))) [])
       #_(remove empty?)))

    ;; and 2 helper functions
(defn find-horizontal-sides [region]
  (find-sides region y-coord x-coord))

(defn find-vertical-sides [region]
  (find-sides region x-coord y-coord))

(comment
  (find-vertical-sides #{[0 6] [0 5] [3 4] [1 4] [1 3] [1 5] [0 3] [2 4] [0 2] [0 4] [1 6] [1 2] [3 5]})

  (group-by second [[1.25 3] [1.25 5] [1.25 6] [1.25 2]])
  ;;
  )

(defn fences-in-sides [horiz-sides vertical-sides]
  (reduce into #{} (concat horiz-sides vertical-sides)))

(defn describe-fences-and-sides [region]
  (let [all-fences       (find-fences region)
        horizontal-sides (find-horizontal-sides region)
        vertical-sides   (find-vertical-sides region)
        occupied-fences  (fences-in-sides horizontal-sides vertical-sides)]
    {:all-fences all-fences
     :h horizontal-sides
     :v vertical-sides
     :occ occupied-fences
     :standalone-fences (remove occupied-fences all-fences)}))

(defn compute-fence-side-price [region]
  (let [{:keys [h v standalone-fences]} (describe-fences-and-sides region)]
    (+ (count h)
       (count v)
       (count standalone-fences))))


(defn solution-2 [input]
  (->> (create-garden input)
       find-all-regions
       (map (juxt count compute-fence-side-price))
       (map #(apply * %))
       (reduce +)))


(comment
  (solution-2 sample-input-1)
  ;; => ok
  (solution-2 sample-input-2)
  ;; => ok

  (solution-2 "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE
")
  ;; => 236 .. ok

  (solution-2 "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
")
   ;; => 368 ok

  (solution-2 sample-input-3)
  ;; => 1263  not ok 😭 
  ;; expected result is 1206 

  ;; let's find out why this sample-input-3 does not returns the correct price

  (def detail-results  (let [garden (create-garden sample-input-3)]
                         (->> garden
                              find-all-regions
        ;; decorate with plant
                              (map (juxt (comp (partial plant-at garden) first)
                                         #(assoc {} :region %)))
         ;; add fences and sides info
                              (map (fn [[plant {:keys [region] :as info}]]
                                     [plant (assoc info :fences (describe-fences-and-sides region))]))
                              (map (fn [[plant {:keys [fences region] :as info}]]
                                     (let [count-region (count region)
                                           count-h-sides (count (:h fences))
                                           count-v-sides (count (:v fences))
                                           count-fences  (count (:standalone-fences fences))
                                           total-fence-price (+ count-h-sides count-v-sides count-fences)]
                                       (println (format "A region of %s plants with price %d * %d = %d" plant count-region total-fence-price (* count-region total-fence-price))))

                                     [plant info]))
                              (into {}))))

;; => actual   : A region of V plants with price 13 * 11 = 143
;; => expected : A region of V plants with price 13 * 20 = 260

;; => actual   : A region of J plants with price 11 * 16 = 176
;; => expected : A region of J plants with price 11 * 20 = 220

  (def garden [[\R \R \R \R \I \I \C \C \F \F]
               [\R \R \R \R \I \I \C \C \C \F]
               [\V \V \R \R \R \C \C \F \F \F]
               [\V \V \R \C \C \C \J \F \F \F]
               [\V \V \V \V \C \J \J \C \F \E]
               [\V \V \I \V \C \C \J \J \E \E]
               [\V \V \I \I \I \C \J \J \E \E]
               [\M \I \I \I \I \I \J \J \E \E]
               [\M \I \I \I \S \I \J \E \E \E]
               [\M \M \M \I \S \S \J \E \E \E]])

  ;; first check the region of V 
  (def region-of-v (get-in detail-results [\V :region]))
  (do
    (println "REgion of V : ")
    (println (format "regions area : %d" (count region-of-v)))
    (println (format "total fences : %d" (count (get-in detail-results [\V :fences :all-fences]))))
    (let [h-sides-count (count (get-in detail-results [\V :fences :h]))
          v-sides-count (count (get-in detail-results [\V :fences :v]))
          standalone-fences-count (count (get-in detail-results [\V :fences :standalone-fences]))]
      (println (format "horiz sides : %d" h-sides-count))
      (println (format "vert  sides : %d" v-sides-count))
      (println (format "      total : %d" (+ h-sides-count v-sides-count)))
      (println (format "remaining fences : %d" standalone-fences-count))))
  
  ;; the count of vertical sides is INCORRECT 😠
  ;; it is 3 but should be 4

(find-vertical-sides #{[0 6] [0 5] [3 4] [1 4] [1 3] [1 5] [0 3] [2 4] [0 2] [0 4] [1 6] [1 2] [3 5]})

  )
