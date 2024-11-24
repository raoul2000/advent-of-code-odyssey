(ns day-10-part-2
  (:require [day-10 :refer [find-possible-next-steps
                            create-grid
                            in-grid
                            find-S-pos
                            walk-the-pipes
                            sample-input-1
                            sample-input-2]]))

;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We must count how many tiles are enclosed in the loop

;; After some search I tought to implement a flood fill algorithm
;; - BFS
;; - fill tiles *outside* of the loop
;; There is however a particular condition that we must consider if we want to
;; flood fill all tiles *outside* of the loop and thaht is loop pipes sticked to the
;; edge of the grid.

;; Imagine this loop :
;; .....
;; .F-7.
;; .|.|.
;; .L-J.
;; .....

;; Starting from [0 0] flood fill will work well. But now what about with this loop :

;; .F-7.
;; .|.|.
;; .L-J.

;; There is no was that, starting from [0 0] we can end up filling the right most column 
;; simply because the algo will not access it.
;; One option would be to add a frame of tiles around the existing grid so to connect together
;; all outside regions.

(def sample-input-2-1 "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
")

(def sample-input-2-2 ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
")

(comment
  (create-grid sample-input-1)

  ;; expand grid 
  (-> (create-grid sample-input-1)
      ;; add left and right cols
      (update :matrix (partial mapv #(into [\.] (conj % \.))))
      (update :matrix #(into [[1 2 3]] (conj % [4 5 6]))))

  ;; Create a function for it
  )

(defn expand-grid [grid]
  (let [extra-row (apply vector (repeat (+ 2 (:col-count grid)) \.))]
    (-> grid
        ;; add extra cols
        (update :matrix (partial mapv #(into [\.] (conj % \.))))
        ;; add extra rows
        (update :matrix #(into [extra-row] (conj % extra-row)))
        (update :col-count (partial + 2))
        (update :row-count (partial + 2))
        (assoc :frame-size (+ (* 2 (+ 2 (:col-count grid)))
                              (* 2 (:row-count grid)))))))

(comment
  (expand-grid (create-grid sample-input-1))
  ;;
  )


;; Now it seems the solution from part-1 must be modified in order to keep track of all
;; steps composing the loop. Because it was not need by part-1, we used to only remember the previous step
;; but now we need them all.
;; Let's do these modifications then ...

(defn walk-next-step [grid [[_ current-coord] [_ prev-coord] :as all-steps]]
  ;; conj so to keep all steps
  (conj all-steps (->> (find-possible-next-steps grid current-coord prev-coord) ;; 
                       first)))

(defn create-state [input]
  (let [grid            (-> (create-grid input)
                            expand-grid)
        start-pos       (find-S-pos grid)
        [step-1 step-2] (find-possible-next-steps grid start-pos nil)]
    {:grid            grid
     :start-pos       start-pos
     :step-count      1
     :path            [(list step-1) (list step-2)]  ;; use lists instead of vectors
     :pipe-walker-fn  (partial walk-next-step grid)}))

(comment
  (walk-the-pipes (create-state sample-input-1))
  (walk-the-pipes (create-state sample-input-2))
  (walk-the-pipes (create-state (slurp "resources/day_10.txt")))

  ;;
  )

;; With the create-loop function below, having all coord of tiles involed in the loop
;; we can easely define if a coord is part of the loop, outside the grid, or just a
;; regular tile.

(defn create-loop
  "Given puzzle input, returns a state where :
   - `:grid`: is the grid map
   - `:loop`: is a set of all coord involved in the loop"
  [input]
  (let [state (walk-the-pipes (create-state input))]
    {:grid       (:grid state)
     :loop-tiles (-> #{(:start-pos state)}
                     (into (map second (first  (:path state))))
                     (into (map second (second (:path state)))))}))

(comment
  (create-loop sample-input-1)
  (create-loop sample-input-2)
  (create-loop (slurp "resources/day_10.txt"))
  (create-loop sample-input-2-1)
  (create-loop sample-input-2-2)
  ;;
  )

;; For flood fill algo BFS we need a queue
;; see https://admay.github.io/queues-in-clojure/

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(comment

  (def q (queue [1 2 3]))
  (def q2 (queue))
  (empty? q)
  (empty? q2)
  (seq q)
  (conj q :a :b :c)
  (apply conj q [:a :b :c])
  (peek q)
  (pop q)
  (seq q)
  (peek q)

  ;;
  )

;; We are going to use flood fill on all tiles *outside* the loop and count them. Then
;; to get the puzzle solution we will:
;; solution = total tiles count -  tiles in the loop - flood filled tiles 
;; 
;; We must start flood fill from a coord that *we know* is outside the loop.
(def start-1 [0 0])
(def start-2 [0 0])

;; Let's try to implement the BFS flood fill algo

(defn fill-adjacent [{:keys [q visited-tiles loop-tiles grid] :as state}]
  (let [in-grid?                  (in-grid grid)
        [x y :as current-coords]  (peek q)
        adjacent-coords           (filterv in-grid? [[(inc x)  y]
                                                     [(dec x)  y]
                                                     [x  (inc y)]
                                                     [x  (dec y)]])
        not-visited-adj           (remove visited-tiles adjacent-coords)
        not-in-loop-adj           (remove loop-tiles    not-visited-adj)]
    (-> state
        (update :q             pop)
        (update :q             #(apply conj % not-in-loop-adj))
        (update :visited-tiles conj current-coords))))


(defn flood-fill [input start-coord]
  (loop [state (-> (create-loop input)
                   (assoc :q             (queue [start-coord]))
                   (assoc :visited-tiles (hash-set)))]
    (if (empty? (:q state))
      state
      (recur (fill-adjacent state)))))

(comment
  (flood-fill sample-input-2-1 [0 0])
  (flood-fill sample-input-2-2 [0 0])
  ;;
  )

(defn solution-2 [input start-coord]
  (let [final-state         (flood-fill input start-coord)
        visited-tiles-count (count (:visited-tiles final-state))
        loop-tiles-count    (count (:loop-tiles final-state))
        total-tiles-count   (* (get-in final-state [:grid :col-count])
                               (get-in final-state [:grid :row-count]))]
    (- total-tiles-count loop-tiles-count visited-tiles-count)))

(comment
  (solution-2 sample-input-2-1 [0 0])
  ;; => 4 good result

  (solution-2 sample-input-2-2 [0 0])
  ;; => 10 😥 .. this is *not* the expected result which is 8
  ;; We are missing 2 tiles and by observing the sample result it is easy to
  ;; understand where are they located:

;;     ||
;;  OF----7F7F7F7F-7OOOO
;;  O|F--7||||||||FJOOOO
;;  O||OFJ||||||||L7OOOO <
;;  FJL7L7LJLJ||LJIL-7OO
;;  L--JOL7IIILJS7F-7L7O <
;;  OOOOF-JIIF7FJ|L7L7L7
;;  OOOOL7IF7||L7|IL7L7|
;;  OOOOO|FJLJ|FJ|F7|OLJ
;;  OOOOFJL-7O||O||||OOO
;;  OOOOL---JOLJOLJLJOOO

;; See [3 2] and [4 4] ? They can't be reached by the flood fill algorithm so they are missing in
;; the final count. 
;; The solution could be to find adjacent tiles also in diagonal directions but if this would indeed 
;; fix [4 4] it doesn't fix [3 2] which is isolated in an island.
;; The floodfill algo implemented fails on island regions.


  ;; we must think of another way to solve this one !

  ;; Based on this post (https://gamedev.stackexchange.com/questions/141460/how-can-i-fill-the-interior-of-a-closed-loop-on-a-tile-map)
  ;; we can try to scan each line of the grid and extract boundaries between external in internal network tiles.
  ;; pseudo code:
  ;; result = 0
  ;; lineTileCount = 0
  ;; for each tiles of a given line (from left to right)
  ;;    if the tile belongs to the network
  ;;       if the tile is an horizontal pipe,
  ;;          just ignore it
  ;;       if the tile is a pipe (i.e no horizontal one) or 'S'
  ;;          store it in the boundaries list
  ;;    else, 
  ;;       if boundaries is not empty
  ;;           lineTileCount = lineTileCount + 1
  ;;    
  ;;    if the boundaries list contains 2 coords
  ;;       result = result + lineTileCount
  ;;       lineTileCount = 0
  ;;       clear boundaries list
  ;;
  ;; We could improve this algo by replacing the boundraies list with some flag called inside-network, but for debug
  ;; purposes it could be more intresting to keep track of boundaries points.

  (def state (create-loop sample-input-2-1))

  ;; So, we are going to process all tiles, line by line, and for each one we will need
  ;; - its coords (to check if it is part of the loop)
  ;; - its content char 




  (defn coords-by-line [{:keys [col-count row-count]}]
    (partition col-count (for [y (range 0 row-count)
                               x (range 0 col-count)]
                           [x y])))

  (coords-by-line (:grid state))

  ;; Let's create some more helpers function

  (defn in-loop? [{:keys [loop-tiles]} pos]
    (loop-tiles pos))

  (in-loop? state [0 0])
  (in-loop? state [8 8])

  (def horizontal-pipe? (partial = \-))
  (horizontal-pipe? \c)
  (horizontal-pipe? \-)

  (defn is-pipe? [c]
    (#{\| \- \L \J \7 \F \S} c))
  (is-pipe? \i)
  (is-pipe? \L)

  (defn char-at [grid [x y]]
    (-> (:matrix grid)
        (nth y)
        (nth x)))

  (char-at (:grid state) [0 11])

  ;; Create a 2d array (grid) where each cell is a triplet:
  ;; - tiles coords [x y]
  ;; - char at pos
  ;; - belong-to-loop flag
  ;; - is-pipe flag
  ;; - is-horizontal-pipe flag

  (map (fn [line]
         (map (fn [[x y :as pos]]
                (let [c (char-at (:grid state) [x y])]
                  (vector pos
                          c
                          (boolean (in-loop? state pos))
                          (boolean (is-pipe? c))
                          (boolean (horizontal-pipe? c)))))  line))
       (coords-by-line (:grid state)))


  ;; Below we extract boundaries coords pairs for each line

  (map (fn [line]
         (->> (reduce (fn [acc pos]
                        (let [c           (char-at (:grid state) pos)
                              is-boundary (and (in-loop? state pos)
                                               (not (horizontal-pipe? c)))]
                          (cond-> acc
                            is-boundary (conj pos))))
                      []
                      line)
              (partition 2)))
       (coords-by-line (:grid state)))

  ;; But then we can't just count tiles between thoses boundaries pairs because they also include
  ;; horizontal pipes, anb we don't want to count them

  (map (fn [line]
         (->> (reduce (fn [acc pos]
                        (let [c           (char-at (:grid state) pos)
                              is-boundary (and (in-loop? state pos)
                                               (not (horizontal-pipe? c)))]
                          (cond-> acc
                            is-boundary (conj pos))))
                      []
                      line)
              (partition 2)
              (map (fn [[[x1 y1] [x2 y2]]] 
                     (mapv #(char-at (:grid state) (vector % y1)) (range (inc x1) x2))
                     ))
              ))
       (coords-by-line (:grid state)))


  ;; It seems we have enough to implement the algo : each line of the above created grid should be reduced to the number
  ;; of internal tiles for this line

  #_(map (fn [line]
           (reduce
            (fn [{:keys [inner-count boundaries inside] :as acc} [[x y] _c in-loop is-pipe is-horizontal-pipe]]
              (-> acc
                  (update :inner-count (fn [prev]
                                         (if (and (not in-loop)
                                                  inside)
                                           (inc prev)
                                           prev)))
                  (update :inside (fn [prev-inside]
                                    (cond
                                      (and in-loop (not is-horizontal-pipe)))))))
            {:inner-count 0
             :inside      false
             :boundaries []}
            line)))






  ;; Ok, so we need a function to process a line of the grid. It will receive as input a predicates to define if a
  ;; tile is included in the network path or not

  (defn scan-grid-line [line part-of-loop? char-at]
    (reduce (fn [{:keys [count-enclosed boundaries :as state]} pos]
              (-> state
                  (update :count-enclosed inc)))
            {:count-enclosed 0
             :boundaries     []} line))

  (defn solution-2-b [input]
    (let [state         (create-loop input)
          is-in-loop?   (partial in-loop? (:loop-tiles state))
          char-at-coord (partial char-at (:grid state))
          char-at-tiles (fn [[x y]] (-> (get-in state [:grid :matrix])
                                        (nth y)
                                        (nth x)))]
      (->> (coords-by-line (:grid state))
           (map (fn [line]

                  (reduce (fn [acc pos]
                            (tap> pos)
                            (-> acc
                                (update :count inc)
                                (update :boundaries conj (char-at-tiles pos))))
                          {:count 0
                           :boundaries []} line))))))

  (solution-2-b sample-input-2-1)




  ;;
  )
