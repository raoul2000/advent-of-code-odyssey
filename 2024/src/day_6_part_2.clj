(ns day-6-part-2
  (:require [clojure.string :as s]
            [criterium.core :refer [bench benchmark]]))

;; first attempt to solve part 2 failed (see day_6.clj) because computation time exploded 💥
;; let's try with a new approach.
;;
;; We want to :
;; compute the initial guard steps (from part-1)
;; For each step, put an obstruction
;;   - compute new guard path an detect a loop


(def sample-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")

(def puzzle-input (slurp "resources/day_6.txt"))

(defn create-grid [s]
  (->> (s/split-lines s)
       (mapv vec)))

(defn grid-size
  "Returns [col-count line-count] for the given grid"
  [grid]
  [(count (first grid)) (count grid)])

(defn read-char-at [grid [x y]]
  (get-in grid [y x]))

(defn in-grid?
  "Return TRUE if pos [x y] is inside the given `grid`"
  [grid [x y]]
  (let [[col-count line-count] (grid-size grid)]
    (and (< -1 x col-count)
         (< -1 y line-count))))

(defn write-char-at
  "Returns `grid` where an obstruction character is added at position `[x y]`.
   Returns `grid` with no change when position is out of grid.
   "
  [grid [x y] c]
  (if (in-grid? grid [x y])
    (update-in grid [y x] (constantly c))
    grid))

(defn find-starting-pos [grid]
  (let [[col-count line-count] (grid-size grid)]
    (first (drop-while #(not= \^ (read-char-at grid %)) (for [x (range 0 col-count)
                                                              y (range 0 line-count)]
                                                          [x y])))))

(defn move [current-pos direction]
  (mapv + current-pos direction))

(comment
  (move [1 1] [0 -1])
  ;;
  )
(defn rotate-direction-90 [[dx dy]]
  (case [dx dy]
    [0 -1]  [1  0]
    [1  0]  [0  1]
    [0  1]  [-1 0]
    [-1 0]  [0 -1]))

(def dir-up \^)
(def dir-right \>)
(def dir-down \v)
(def dir-left \<)

(def valid-dir? #{dir-up dir-right dir-down dir-left})

(defn direction->char [dir]
  (case dir
    [0 -1] dir-up
    [1  0] dir-right
    [0  1] dir-down
    [-1 0] dir-left
    (throw (ex-info "invalid direction" {:dir dir}))))

(defn find-next-step [current-pos current-dir grid]
  (loop [new-dir current-dir]
    (let [next-pos (move current-pos new-dir)]
      (if-not (#{\# \O} (read-char-at grid next-pos))
        [next-pos new-dir]
        (recur (rotate-direction-90 new-dir))))))

(comment
  (find-next-step [4 6] [0 -1] (create-grid sample-input))
  ;;
  )

(defn build-path [grid start-pos]
  (loop [path {:cur-pos start-pos
               :cur-dir [0 -1]
               :pos  [start-pos]
               :grid grid}]
    (let [cur-pos  (:cur-pos path)
          cur-dir  (:cur-dir path)
          grid     (:grid    path)]
      (if-not (in-grid? grid cur-pos)
        path
        (let [[next-pos next-dir] (find-next-step cur-pos cur-dir grid)]
          (recur (-> path
                     (update :pos conj next-pos)
                     (assoc :cur-pos next-pos)
                     (assoc :cur-dir next-dir)
                     (update :grid write-char-at next-pos (direction->char next-dir)))))))))

(defn grid->str [grid]
  (->> (map (fn [line]
              (map str line)) grid)
       (map #(apply str %))))

(defn count-guard-pos [grid]
  (->> (map (fn [line]
              (count (filter valid-dir? line))) grid)
       (apply +)))

(defn solution-1 [input]
  (let [grid      (create-grid input)
        start-pos (find-starting-pos grid)]
    (count-guard-pos (:grid (build-path grid start-pos)))))

(comment
  (solution-1 sample-input)
  ;; => 41 (good)
  (solution-1 puzzle-input)
  ;; 5312 ⭐

  (time (solution-1 puzzle-input))
  ;; "Elapsed time: 26.1973 msecs"
  ;; This is muuuch better than the initial solution 1
  ;;
  )

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now that we have a better solution for part 1, let's see what we can do for part 2

(defn in-loop? [grid pos dir]
  (= (read-char-at grid pos)
     (direction->char dir)))

(defn build-path-with-loop-detection [grid start-pos]
  (loop [path {:cur-pos start-pos
               :cur-dir [0 -1]
               :pos     [start-pos]
               :grid    grid
               :loop?   false}]
    (let [cur-pos  (:cur-pos path)
          cur-dir  (:cur-dir path)
          grid     (:grid    path)
          in-loop  (:loop?   path)]
      (cond
        (not (in-grid? grid cur-pos))  path

        in-loop                       path #_(do
                                        (print cur-pos)
                                        path)
        :else
        (let [[next-pos next-dir] (find-next-step cur-pos cur-dir grid)]
          (recur (-> path
                     (update :pos conj next-pos)
                     (assoc  :cur-pos next-pos)
                     (assoc  :cur-dir next-dir)
                     (assoc :loop? (in-loop? grid next-pos next-dir))
                     (update :grid write-char-at next-pos (direction->char next-dir)))))))))

(comment
  (def initial-grid (create-grid puzzle-input))
  (def start-pos (find-starting-pos initial-grid))
  (def guard-path (:pos (build-path-with-loop-detection initial-grid start-pos)))

  (count guard-path)
  (count (set guard-path))

  (def path-1 (->> (map #(write-char-at initial-grid % \O) (set guard-path))
                   (map #(build-path-with-loop-detection % start-pos))
                   (filter :loop?)
                   count
                   #_(take 8)
                   #_last))

  ;; 1748 !!
  ()
  (grid->str (:grid path-1))
  

  (build-path-with-loop-detection (write-char-at (create-grid sample-input) [3 6] \#) [4 6])





  ;;
  )