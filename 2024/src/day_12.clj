(ns day-12
  (:require [clojure.string :as s]))


;; https://adventofcode.com/2024/day/12

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

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

;; dealing with grid ? yes, so we need our usual function to manipulate grid
;; with some naming work : grid = garden


(defn create-garden [s]
  (->> (s/split-lines s)
       (mapv #(mapv identity  (vec %)))))

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

(comment
  (create-garden sample-input-1)
  ;;
  )

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

(defn print-garden
  "Print given `garden` as string to stdout."
  [garden]
  (->> garden
       (map (partial apply str))
       (s/join "\n")
       print))

(defn set-at-pos [garden [x y] c]
  (update-in garden [y x] (constantly c)))

(defn mark-region
  "Returns the given `garden` where each position in the `region` is marked with character `c`."
  [garden region c]
  (reduce (fn [acc pos]
            (set-at-pos acc pos c))
          garden
          region))

(comment
  (def garden (create-garden puzzle-input))
  (print-garden (set-at-pos garden [50 0] \_))

  (def garden (create-garden sample-input-1))

  (defn zoom [garden zoom-factor]
    (reduce (fn [acc row]
              (let [col-gap   (apply str (repeat (dec zoom-factor) "."))
                    inner-row (->> row
                                   (interpose col-gap)
                                   (apply str))
                    with-margin (str "." inner-row ".")]
                (conj acc with-margin))) [] garden))

  (print-garden (zoom garden 3))
  (str "." "rrr")
  (repeat 2 ".")
  (conj [] [1 2] [4 5])


  (apply str (interpose "." "ABC"))

  (print-garden garden)
  (print-garden (mark-region garden (region-at-pos garden [20 20]) \.))
  (print-garden (reduce (fn [acc pos]
                          (set-at-pos acc pos \.))
                        garden
                        (region-at-pos garden [10 0])))

  (print-garden garden)
  (find-adjacent-pos garden [1 0])

  (region-at-pos garden [0 0])
  (region-at-pos garden [0 1])
  (region-at-pos garden [2 1])

  ;;
  )

(defn all-pos
  "Returns a seq of all pos in the given `garden`, from top-left to bottom-right."
  [garden]
  (let [[col-count line-count] (garden-size garden)]
    (for [x (range 0 col-count)
          y (range 0 line-count)]
      [x y])))

(comment
  (all-pos (create-garden sample-input-1))
  (all-pos (create-garden sample-input-2))
  ;;
  )

(defn already-visited? [garden pos]
  (= \. (plant-at garden pos)))

(defn mark-region-visited [garden region]
  (mark-region garden region \.))


(defn find-all-regions
  "Given a `initial-garden` returns a seq of all regions in thie garden."
  [initial-garden]
  (:regions (reduce (fn [{:keys [garden regions] :as state} pos]
                      (if (already-visited? garden pos)
                        state
                        (let [new-regions (region-at-pos garden pos)]
                          (-> state
                              (assoc  :garden  (mark-region-visited garden new-regions))
                              (update :regions conj new-regions)))))
                    {:garden initial-garden
                     :regions []}
                    (all-pos initial-garden))))

(comment
  (find-all-regions (create-garden sample-input-1))
  ;;
  )

;; now, given a regions, what is its fence size ?
;; if the [x y] check all adjacent pos and count the ones which are NOT in the region


(defn create-fence [region]
  (->> (mapcat  touching-pos-xs  region)
       (remove region)))

(comment
  (def garden (create-garden sample-input-1))

  (count (create-fence (region-at-pos garden [0 0])))
  (count (create-fence (region-at-pos garden [0 1])))
  (count (create-fence (region-at-pos garden [3 1])))
  (count (create-fence (region-at-pos garden [2 1])))

  ;;
  )

(defn compute-fence-price [region]
  (count (create-fence region)))

(defn solution-1 [input]
  (->> (create-garden input)
       find-all-regions
       (map (juxt count compute-fence-price))
       (map #(apply * %))
       (reduce +)))


(comment
  (solution-1 sample-input-1)
  ;; => 140 good
  (solution-1 sample-input-2)
  ;; => 772 ...better
  (solution-1 sample-input-3)
  ;; => 1930 .. smells good

  (solution-1 puzzle-input)
  ;; => 1431440 ..yess ! ⭐
  )

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; introducing the fence bulk-discount
;; - count fence side instead of fence unit

(comment
  (def garden (create-garden sample-input-1))
  (print-garden garden)
  (map create-fence (find-all-regions garden))

;; given a fence represented as a seq of pos a side is a sub seq having same x (or y)
  
  (def reg-1 #{[0 0] [1 0] [3 0] [2 0]})
  (def fences-1 (create-fence reg-1))


  ;; group by x (vertical fences)
  ;; map fo fences :
  ;; - k : x coord for starting horiz fence
  ;; - v : pos of fences with this x coordinate
  (group-by first fences-1)
  ;; => {-1 [[-1 0]], 0 [[0 1] [0 -1]], 1 [[1 1] [1 -1]], 4 [[4 0]], 3 [[3 1] [3 -1]], 2 [[2 1] [2 -1]]}
  ;; We have 2  single fences (on x = -1, 4)  for sure they are not sides
  ;; ...and 4 Double fences ofr x = 0, 1 3 2 ...they are potential sides
  
  ;; group-by y (horizontal fences)
  ;; map of fences :
  (group-by second fences-1)
  ;; => {0 [[-1 0] [4 0]], 1 [[0 1] [1 1] [3 1] [2 1]], -1 [[0 -1] [1 -1] [3 -1] [2 -1]]}
  ;; We have one group of 2 fences on y = 0 
  ;; ... and 2 groups of 4 fences for y = 1 and y = -1
  ;; So we may have up to 3 sides
  
  ;; how to find if a seq of fences pos having the same x (or y) coordinates, are consecutive ?
  ;; Or in other words, how to find all sub sequence of fence consecutive x (or y) coordinates ?
  
  (def x-coord first)
  (def y-coord second)

  ;; let work on horizontal fence
  ;; sort it on x axis (they have same y anyway)
  (def horiz-fences-1 (sort-by x-coord [[0 1] [1 1] [3 1] [2 1]]))

  ;; lets find horizontal sides (y = constant)
  (defn f-side [fences]
    (reduce (fn [acc fence]
              (if (empty? acc)
                ;; the first side
                (conj acc [fence])
                ;; is this fence consecutive to the last fence of the last side ?
                (let [prev-fence (last (last acc))]
                  (if (= (inc (x-coord prev-fence)) (x-coord fence))
                  ;; consecutive
                    (-> acc
                        butlast
                        (conj (conj (last acc) fence)))
                    ;; not consecutive : start new side
                    (conj acc [fence])))))
            []
            fences))
  (f-side horiz-fences-1)
  ;; now with non consecutive sides
  (f-side (sort-by x-coord [[0 1] [1 1] [4 1] [3 1]]))
  ;; => ([[4 1]] [[3 1]] [[0 1] [1 1]])
  ;; We get 2 sides
  
  ;; ..and now with no sides at all
  (f-side (sort-by x-coord [[0 1] [6 1] [4 1] [2 1]]))
  ;; 4 items => no sides found here
  
;; to do the same for vertical sides, rewrite the function
  (defn f-side-2 [fences axis-fn]
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
            (sort-by axis-fn fences)))
  
  (f-side-2 [[0 1] [1 1] [4 1] [3 1]] x-coord)
  (f-side-2 [[0 1] [6 1] [4 1] [2 1]] x-coord)
    ;; let's try with vertical sides
  (f-side-2 [[2 1] [2 -1]] y-coord)
  (f-side-2 [[2 1] [2 0] [2 4] [2 3]] y-coord)


  
  (f-side-2 [[-1 0]] x-coord)
    ;; good !
  
    ;; Now , for a given region we can get its fences and from it, all the horizontal and vertical sides
  (let [fences (create-fence reg-1)
        horiz-sides (f-side-2 fences x-coord)
        vert-sides  (f-side-2 fences y-coord)
        ]
    {:h horiz-sides
     :v vert-sides}
    #_(concat horiz-sides vert-sides)
    )
  




;; ..and consecutive y (or x)
  
;; sort by y
  (def fence-1 [[1 2] [1 -1] [1 3]])
  (sort-by second fence-1)

  (->> fence-1
       (sort-by second)
       (reduce (fn [res [x y]]
                 (if-let [prev-side (last res)]
                   ;; there is a side being built
                   (let [[_x last-fence-y] (last prev-side)]
                     (if (= (inc last-fence-y) y)
                     ;; add tu prev side
                       (-> res
                           butlast
                           (conj (conj prev-side [x y])))
                       ;; start a new side   
                       (-> res
                           (conj [[x y]]))))
                   ;; first side start
                   (conj res [[x y]]))) []))

  ;; we could use the same reducer for both x and y axis
  ;; let"s re-write
  )

(defn sides
  "Given a `region` provided as a set of coords, returns a seq of all sides aounrd this region.
   Sides with a length of 1 are simple fences."
  [region])

(defn successive-fences-horiz? [[f1-x _f1-y]  [f2-x _f2-y]]
  (= (inc f1-x) f2-x))
(defn successive-fences-vertical? [[_f1-x f1-y]  [_f2-x f2-y]]
  (= (inc f1-y) f2-y))


(defn fence-side-reducer [successive-fences?]
  (fn [res [x y]]
    (if-let [prev-side (last res)]
      (if (successive-fences? (last prev-side) [x y])
          ;; add to prev side
        (-> res
            butlast
            (conj (conj prev-side [x y])))
          ;; start a new side   
        (-> res
            (conj [[x y]])))
         ;; first side start
      (conj res [[x y]]))))


(comment
  ;; horozontal sides for fence-1
  (def horiz-sides (->> fence-1
                        (sort-by first)
                        (reduce  (fence-side-reducer successive-fences-horiz?) [])))

  ;; vertical sides for fence-1
  (def vert-sides (->> fence-1
                       (sort-by second)
                       (reduce  (fence-side-reducer successive-fences-vertical?) [])))

    ;; all remaining fences of length 1 (i.e. not involved in any side)
    ;; must be de-dup because they are counted once for vertical and once for horiz

  (count (set (concat horiz-sides vert-sides)))

;;
  )

(defn compute-fence-side-price [region]
  (let [fences (create-fence region)
        horiz-sides (->> fences
                         (sort-by first)
                         (reduce  (fence-side-reducer successive-fences-horiz?) []))
        vert-sides (->> fences
                        (sort-by second)
                        (reduce  (fence-side-reducer successive-fences-vertical?) []))]
    {:h horiz-sides
     :v vert-sides}
    #_(->> (reduce (fn [acc side]
                     (if (>  (count side) 1)
                       (update acc :sides  conj side)
                       (update acc :fences conj (first side))))
                   {:sides []
                    :fences #{}}
                   (concat horiz-sides vert-sides))
           #_(map #(count (second %))))))

(comment
  (print-garden (create-garden sample-input-1))

  (->> (create-garden sample-input-1)
       find-all-regions
       #_(map compute-fence-side-price))

  (compute-fence-side-price #{[0 0] [1 0] [3 0] [2 0]})
  (->> '([[-1 0] [0 1] [1 1] [2 1] [3 1] [4 0]]
         [[0 -1] [1 -1] [2 -1] [3 -1]]
         [[2 1]]
         [[3 1]]
         [[1 1]]
         [[0 1]]
         [[3 -1] [4 0]]
         [[2 -1] [-1 0]]
         [[0 -1]]
         [[1 -1]])
       (reduce (fn [acc side]
                 (if (>  (count side) 1)
                   (update acc :s conj side)
                   (update acc :f conj (first side)))) {:s [] :f #{}}))
  ;;
  )



(defn solution-2 [input]
  (->> (create-garden input)
       find-all-regions
       (map (juxt count compute-fence-side-price))
       (map #(apply * %))
       (reduce +)))


