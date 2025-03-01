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

  ;; let's parametrize our function to accept axis (horiz or vertical)
  (defn f-side-2 [axis-fn fences-by-axis]
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
            (sort-by axis-fn fences-by-axis)))

  (f-side-2 x-coord [[0 1] [1 1] [3 1] [2 1]])
  (f-side-2 x-coord [[0 1] [6 1] [4 1] [2 1]])
    ;; let's try with vertical sides
  (f-side-2 x-coord [[2 1] [2 -1]])
  (f-side-2 x-coord [[2 1] [2 0] [2 4] [2 3]])
  (f-side-2 x-coord [[-1 0] [4 0]])

  (f-side-2 x-coord [[-1 0]])
    ;; good !

    ;; Now , for a given region we can get its fences and from it, all the horizontal and vertical sides

  (def reg-3 #{[0 0] [1 0] [3 0] [2 0] [0 1] [1 1]})
  (def h-sides (->> (create-fence reg-3)
                    (group-by y-coord)
                    (map second)
                    ;; keep only fences having the same y-coord
                    (remove #(= 1 (count  %)))
                    ;; sides are fences with successive x-coord
                    (map #(f-side-2 x-coord %))
                    ;; aggregate sides parts with a length > 1 : they are sides
                    (reduce (fn [acc sides-parts]
                              (->> sides-parts
                                   (remove #(= 1 (count %)))
                                   (into acc))) [])
                    (remove empty?)))
   ;; h-sides => ([[0 -1] [1 -1] [2 -1] [3 -1]] [[2 1] [3 1]] [[0 2] [1 2]])
   ;; This is correct : the region 3 contains 3 horizontal sides 
   ;; Now we must count remaining fences : they are simple not involved in sides

  (def sides-items (reduce into #{} h-sides))
  (->> (create-fence reg-3)
       (remove sides-items))

  ;; actually we should find vertical sides, merge them with horiz sides
  ;; and remove fences not involved in one of them

  ;; let's turn above form into a parametrized function
  (defn find-sides [region group-fn axis-fn]
    (->> (create-fence region)
         (group-by group-fn)
         (map second)
                      ;; keep only fences having the same y-coord
         (remove #(= 1 (count  %)))
                      ;; sides are fences with successive x-coord
         (map #(f-side-2 axis-fn %))
                      ;; aggregate sides parts with a length > 1 : they are sides
         (reduce (fn [acc sides-parts]
                   (->> sides-parts
                        (remove #(= 1 (count %)))
                        (into acc))) [])
         (remove empty?)))

    ;; and 2 helper functions
  (defn find-horizontal-sides [region]
    (find-sides region y-coord x-coord))

  (defn find-vertical-sides [region]
    (find-sides region x-coord y-coord))

  ;; let's test
  (find-horizontal-sides reg-3)
  (find-vertical-sides reg-3)
  ;; good !

  ;; now merge them into a set
  (def fences-in-sides (reduce into #{} (concat (find-horizontal-sides reg-3) (find-vertical-sides reg-3))))

  ;; ...and find fences not involved
  (->> (create-fence reg-3)
       (remove (partial fences-in-sides [])))
  ;; => ([4 0]) .. correct

  ;; good, let's write some code now 

;
  )

(def x-coord first)
(def y-coord second)

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
          (sort-by axis-fn fences-by-axis)))

  ;; let's turn above form into a parametrized function
(defn find-sides [region group-fn axis-fn]
  (->> (create-fence region)
       (group-by group-fn)
       (map second)
       ;; keep only fences having the same y-coord
       (remove #(= 1 (count  %)))
       ;; sides are fences with successive x-coord
       (map #(group-by-side axis-fn %))
       ;; aggregate sides parts with a length > 1 : they are sides
       (reduce (fn [acc sides-parts]
                 (->> sides-parts
                      (remove #(= 1 (count %)))
                      (into acc))) [])
       (remove empty?)))

    ;; and 2 helper functions
(defn find-horizontal-sides [region]
  (find-sides region y-coord x-coord))

(defn find-vertical-sides [region]
  (find-sides region x-coord y-coord))

(defn fences-in-sides [horiz-sides vertical-sides]
  (reduce into #{} (concat horiz-sides vertical-sides)))

(defn describe-fences-and-sides [region]
  (let [all-fences       (create-fence region)
        horizontal-sides (find-horizontal-sides region)
        vertical-sides   (find-vertical-sides region)
        occupied-fences  (fences-in-sides horizontal-sides vertical-sides)]
    {:all-fences all-fences
     :h horizontal-sides
     :v vertical-sides
     :occ occupied-fences}))

(defn compute-fence-side-price [region]
  (let [{:keys [all-fences h v occ]} (describe-fences-and-sides region)
        standalone-fences (remove occ all-fences)]
    (+ (count h)
       (count v)
       (count standalone-fences))))


(defn solution-2 [input]
  (->> (create-garden input)
       find-all-regions
       #_(map compute-fence-side-price)
       (map (juxt count compute-fence-side-price))
       (map #(apply * %))
       (reduce +)))

(comment
  (solution-2 sample-input-1)
  ;; => 80 good

  (solution-2 sample-input-2)
  ;; => 436 better

  (solution-2 sample-input-3)
  ;; => 1073 .. not good

  (solution-2 "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
")

;; => 592 .. nooooo 😭
  )

(comment

  (def sample-input-4 "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
")

  (def reg-A (first (find-all-regions (create-garden sample-input-3))))
  (find-horizontal-sides reg-A)
  (set (find-horizontal-sides reg-A))
  (find-vertical-sides reg-A)

;; I guess the whole solution above for part 2, is wrong because it doesn't work
;; in the case of inner sides, in particular when the inner space is 1 unit large;

  (def sample-input-5 "AAAA
AABA
ABAA
AAAA
")

  (->> sample-input-5
       create-garden
       find-all-regions)

  ;; this is the region with 'A'
  (def reg-4 #{[2 2] [0 0] [1 0] [2 3] [3 3] [1 1] [3 0] [1 3] [0 3] [0 2] [2 0] [3 1] [3 2] [0 1]})

  ;; and the fences for this region is wrong
  (create-fence reg-4)
  ;; is it because of inner spaces ? lets' add b positions
  (def reg-4-filled #{[2 1] [1 2] [2 2] [0 0] [1 0] [2 3] [3 3] [1 1] [3 0] [1 3] [0 3] [0 2] [2 0] [3 1] [3 2] [0 1]})
  (create-fence reg-4-filled)
  ;; yes, when the region contains no inner space, fences computation is ok
  ;; but when there are inner spaces, it is not.

  ;; soooo ... the whole solution-2 (that involves stuff from solution-1) should be reviewed
  ;; we will do that on anoter ns

  ;; meet you there 🖖
  

  ;;
  )
