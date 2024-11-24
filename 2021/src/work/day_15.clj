(ns work.day-15
  (:require [clojure.string :as str]))

;; this has been my first steps in solving day 15
;; once at the end, I realized I had not correctly understood the
;; problem which is in fact a pathfinding puzzle. Game over !
;; ...so basically, I keep this file just in case, but really I'll have
;; to write a brand new solution investigating how to implement
;; Algorithm A* (see https://www.youtube.com/watch?v=-L-WgKMFuhE)


;; part 1 ================

(def test-data "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

;; unlike day 9, this time we'll use a 2 dimensions array to
;; store the matrix.
;; With such data structure, reading any value is nothing more than
;; accessing a nested map (because vectors are map where the key is
;; the item index)

(comment
 ;; for example : 
  (get-in [[:a :b :c]
           [1 2 3]
           [:x :y :y]] [0 0]) ;; :a
  )


;; First let's build the matrix as a vector of vectors of int
(defn parse-data [s]
  (->> (str/split-lines s)
       (map #(into [] (map (fn [c]
                             (Character/digit c 10)) %)))
       (into [])))

(comment
  (parse-data test-data)
  ;;
  )

;; now given a point coordinate, write a function that returns 
;; coordinates of all its orthogonal adjacents. Of course we must
;; not return coordonates out of bounds

;; a helper function
(defn max-coords
  "Returns [y-max x-max] given the matrix m, a vector vectors"
  [m]
  (vector (dec (count m))
          (dec (count (first m)))))

(comment
  (max-coords [[1 2 3]
               [1 2 3]])
  ;; => [1 2]
  )

(defn fn-coords-out-of-bounds? [m]
  (let [[y-max x-max] (max-coords m)]
    (fn [[y x :as coords]]
      (or (some neg? coords)
          (> y y-max)
          (> x x-max)))))

(comment
  (def m1 (parse-data test-data))
  (def max-coords (max-coords m1))

  ((fn-coords-out-of-bounds? m1) [1 2])
  ;; => false
  ((fn-coords-out-of-bounds? m1) [-1 2])
  ;; => true
  ((fn-coords-out-of-bounds? m1) [1 -2])
  ;; => true
  ((fn-coords-out-of-bounds? m1) [6 2])
  ;; => false
  ((fn-coords-out-of-bounds? m1) [6 10])
  ;; => true
  )

(defn adjacent-coords [[y x] m]
  (remove (fn-coords-out-of-bounds? m) [[y (inc x)]
                                        [y (dec x)]
                                        [(inc y) x]
                                        [(dec y) x]]))

(comment
  (def m1 (parse-data test-data))

  (adjacent-coords [0 0] m1)
  ;; => ([0 1] [1 0])
  (adjacent-coords [1 1] m1)
  ;; => ([1 2] [1 0] [2 1] [0 1])
  (adjacent-coords [9 1] m1)
  ;; => ([9 2] [9 0] [8 1])
  )

;; Now the function to read the value given its coordinates and the matrix
(defn read-risk
  "Returns the risk for point at given coordinates, where *coord* is [y x]
   and m the matrix of risks"
  [coords m]
  (get-in m coords))

(comment
  (def m1 (parse-data test-data))
  (read-risk [0 0] m1)
  ;; => 1
  (read-risk [1 1] m1)
  ;; => 3
  (read-risk [3 4] m1)
  ;; => 9
  (read-risk [1 10] m1) ;; out of bounds returns nil
  ;; => nil
  ;;
  )

;; we also need a function to test if the exit point has been reached
(defn fn-point-is-exit? [m]
  (fn [[y x]]
    (and (= (inc y) (count m))
         (= (inc x) (count (first m))))))

(comment
  (def m1 (parse-data test-data))
  ((fn-point-is-exit? m1)  [4 5])
  ;; => false
  ((fn-point-is-exit? m1)  [9 8])
  ;; => false  
  ((fn-point-is-exit? m1)  [9 9])
  ;; => true
  ;;
  )

;; ok so we have a matrix of int, a function to read any int given its
;; coordinates, and a function to returns adjacents coordinates of a point.
;; We are ready to go !
;; Starting at 0,0
;; find all adjacent
;; remove the visited points
;; read values for remaining point
;; select the loest value
;; add the corresponding point coordinates and value to the list of visited points
;; is the selected point the bottom -right point ?
;;  - yes : end now
;;  - no : loop starting from the selected point

;; easy
;; we'll use a map called 'path' to store the way out of the matrix.
;; the special key :current-point will store ..the current position [y x]
;;
;; path : { [y1 x1] risk1
;;          [y2 x2] risk2
;;          :current-point [y2 x2]
;;          etc... }
;; 

(defn visited? [point path]
  (get path point))

(comment
  (visited? [2 4] {[0 0] 1
                   [1 0] 2
                   [2 4] 1})
  ;; => 1 (truthy)
  (visited? [2 5] {[0 0] 1
                   [1 0] 2
                   [2 4] 1})
  ;; => nil
  ;;
  )

(defn find-next-step [path m]
  (let [cur-point  ( :current-point path)
        adj-points (adjacent-coords cur-point m)
        next-point (->> (remove #(visited? % path) adj-points)
                        (map #(vector % (read-risk % m)))
                        (reduce (fn [res [_ risk :as all]]
                                  (if (< risk (second res))
                                    all
                                    res))
                                [[] 10]))]
    (if next-point
      (do 
        (prn next-point)
        (-> path
            (assoc ,,, (first next-point) (last next-point))
            (assoc ,,,  :current-point (first next-point)))
        )
      
      (assoc path :blocked true))))

(comment
  (def m1 (parse-data test-data))

  (find-next-step {[0 0] 1
                   :current-point [1 2]} m1)
  (find-next-step {[0 0] 1
                   [0 1] 1
                   [0 2] 6
                   :current-point [0 2]} m1)
  ;;
  )

(defn solve-part-1 [s]
  (let [m                     (parse-data s)
        point-is-exit?        (fn-point-is-exit? m)
        init-path             {[0 0] (read-risk [0 0] m)
                               :current-point [0 0]}]
    (prn m)
    (prn init-path)
    (loop [path init-path]
      (if (or (point-is-exit? (:current-point path))
              (:blocked path))
        path
        (recur (find-next-step path m))))))

(comment
  (solve-part-1 test-data)

  (solve-part-1 
"123
456
789")

  ;;
  )




