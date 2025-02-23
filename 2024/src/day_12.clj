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
                                   (apply str)
                                   )
                    with-margin (str "." inner-row ".")]
                (conj acc with-margin))) [] garden))

  (print-garden (zoom garden 2))
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
  (map create-fence (find-all-regions garden))

;; given a fence represented as a seq of pos a side is a sub seq having same x (or y)

  (def reg-1 '([-1 0] [0 1] [0 -1] [1 1] [1 -1] [4 0] [3 1] [3 -1] [2 1] [2 -1]))

  ;; group by x
  ;; map fo fences :
  ;; - k : x coord for starting horiz fence
  ;; - v : pos of this horiz fence (possibly not consecutive)
  (group-by first reg-1)

  ;; group-by y
  ;; map of all vertical fences
  (group-by second reg-1)

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


  ;; horozontal sides for fence-1
  (->> fence-1
       (sort-by first)
       (reduce  (fence-side-reducer successive-fences-horiz?) [])
       #_count)

  ;; vertical sides for fence-1
  (->> fence-1
       (sort-by second)
       (reduce  (fence-side-reducer successive-fences-vertical?) [])
       #_count)

    ;; all remaining fences of length 1 (i.e. not involved in any side)
    ;; must be de-dup because they are counted once for vertical and once for horiz




  (partition-by inc [1 3 4])








;;
  )



