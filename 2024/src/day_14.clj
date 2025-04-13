(ns day-14
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.string :as str]))

;; https://adventofcode.com/2024/day/14

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; robot
;; px,py : position of a robot
;; vx,vy : velocity of a robot (in tiles per second)
;; directions : 
;; x -> x+1 right
;;  y -> y+1 down
;; top - left pos : 0,0
;; grid
;; col-count, row-count

(def sample-input "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
")

(def puzzle-input (slurp "resources/day_14.txt"))

;; Data model
;; Robot : 
;;  {:px 37, :py 51, :vx 73, :vy 20}

(defn parse-robot-line
  "Given a string describing a robot, returns a map describing the same robot : 
   
   - `px` : X coordinates of robot position
   - `py` : Y coordinates of robot position
   - `vx` : horizontal velocity
   - `vy` : vertical velocity
   "
  [line]
  (->> (re-matches #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)" line)
       rest
       (map edn/read-string)
       (map #(vector %1 %2) [:px :py :vx :vy])
       (into {})))

(defn parse
  "Returns a seq of maps where each map describes a robot"
  [input]
  (->> input
       str/split-lines
       (map parse-robot-line)))

(comment
  (parse sample-input)
  (parse puzzle-input)
  ;;
  )


(comment
;; moving a robot on the x-axis for sec-count sec, given col-count
;; max-x = (dec col-count) 
;; 

  (defn move [{:keys [px vx] :as _robot} col-count sec-count]
    (let [max-x (dec col-count)
          dx    (+ px (* sec-count vx))]
      (cond
        (< -1 dx col-count) dx
        (> dx max-x)        (mod dx col-count)
        :else :boo)))

  (move {:px 0 :vx 10} 6 2)
  (move {:px 0 :vx 6} 6 1)

  ;; dealing with negative velocity
  (def r1 {:px 2 :vx -3})
  (def max-x 5)

  ;; interpolate coordinates to get a positive velocity
  ;; For X axis, left->right is replaced by right->left
  (def r1-i (if (neg-int? (:vx r1))
              (-> r1
                  (update :vx abs)
                  (update :px #(- max-x %)))
              r1))

  ;;
  )


(defn apply-positive-velocity
  "Compute and returns the new coordinate on a single axis given a **positive velocity** value after 
   a given number of seconds."
  [coord positive-velocity axis-size sec-count]
  (let [max-coord   (dec axis-size)
        new-coord   (+ coord (* sec-count positive-velocity))]
    (cond
      (< -1 new-coord axis-size)  new-coord
      (> new-coord max-coord)    (mod new-coord axis-size))))

(comment
  (apply-positive-velocity 0 1 2 1)
  (apply-positive-velocity 0 1 2 2)
  ;;
  )

(defn move-on-axis
  "Compute and returns the new coordinate on a single axis given a **velocity**, after a given amout of seconds.
   
   Note that the *velocity* can be positive or negative integer.
   "
  [coord velocity axis-size sec-count]
  (let [interpol-coord (cond->> coord
                         (neg? velocity)   (- (dec axis-size)))

        new-coord      (apply-positive-velocity  interpol-coord
                                                 (abs velocity)
                                                 axis-size
                                                 sec-count)]
    (cond->> new-coord
      (neg? velocity)  (- (dec axis-size)))))

(comment
  (move-on-axis  0 1 2 2)
  (move-on-axis  0 1 6 1)
  ;;
  )

(defn move-robot
  "Given a map describing a single robot, returns a new map with coordinates updated by aplying the move
   of given velocities during the given amount of seconds."
  [{:keys [vx vy] :as robot} col-count row-count sec-count]
  (-> robot
      (update :px #(move-on-axis % vx col-count sec-count))
      (update :py #(move-on-axis % vy row-count sec-count))))

;; in order to verify with the provided example, let's play with grid
;; - create a grid
;; - place robots on the grid

(defn create-grid
  "Helper function to create a grid given its dimensions. Each grid pos
   is filled with character '.'."
  [col-count row-count]
  (vec (repeat row-count (vec (repeat col-count ".")))))


(defn grid->str [grid]
  (->> (map (fn [line]
              (map str line)) grid)
       (map #(apply str %))))

(defn print-grid [grid]
  (print (s/join "\n" (grid->str grid))))

(defn set-at-pos [grid x y]
  (update-in grid [y x] #(if (= % ".") 1 (inc %))))

(comment
  (def grid (create-grid 11 7))
  (print-grid (set-at-pos grid 0 0))
  (print-grid (set-at-pos grid 1 1))
  (print-grid (set-at-pos (set-at-pos grid 1 1) 1 1))

  ;;
  )

(defn place-robot [grid {:keys [px py] :as _robot}]
  (set-at-pos grid px py))

(comment
  (def col-count 11)
  (def row-count 7)
  (def grid (create-grid col-count row-count))
  (update-in grid [0 9] (constantly "X"))

  (def final-grid (->> sample-input
                       parse
                       (map (fn [robot]
                              (move-robot robot col-count row-count 100)))
                       (reduce (fn [grid robot]
                                 (place-robot grid robot)) grid)))

  (print-grid final-grid)
  (def expected-result "......2..1.
...........
1..........
.11........
.....1.....
...12......
.1....1....
")
  ;; seems to be ok !! 👍
  ;; now we must split the grid into 4 quadrants
  ;; Before placing robots inside the grid, we will sort them by quadront
  ;; removing all robots located on quadront fronter

  ;; Given col-count and row-count
  (int (math/ceil (/ 103 2)))
  (int (math/ceil (/ 101 2)))

  (def x-fronter (int (math/ceil (/ col-count 2))))
  (def y-fronter (int (math/ceil (/ row-count 2))))

  ;; up-left : -1 < x < col-count / 2
  ;;     and : -1 < y < row-count / 2

  ;;
  )

(defn q1? [{:keys [x-fronter y-fronter]} {:keys [px py]}]
  (and (< -1 px x-fronter)
       (< -1 py y-fronter)))

(defn q2? [{:keys [x-fronter y-fronter col-count]} {:keys [px py]}]
  (and (< x-fronter px col-count)
       (< -1 py y-fronter)))

(defn q3? [{:keys [x-fronter y-fronter  row-count]} {:keys [px py]}]
  (and (< -1 px x-fronter)
       (< y-fronter py row-count)))

(defn q4? [{:keys [x-fronter y-fronter  col-count row-count]} {:keys [px py]}]
  (and (< x-fronter px col-count)
       (< y-fronter py row-count)))

(defn grid-spec
  "Given a map describing a grid, returns a new map with 2 new keys : 
   
   - `x-fronter` : value of the X axis representing the vertical quadrant fronter
   - `y-fronter` : value of the Y axis representing the horizontal quadrant fronter
   
   "
  [col-count row-count]
  {:x-fronter (quot col-count 2)
   :y-fronter (quot row-count 2)
   :col-count col-count
   :row-count row-count})

;; Now given a x y pos, assign a quadrant

(defn assign-by-quadrant
  "Given a map describing a robot, returns a new map with extra key `:quadrant` added. Its value
   is the quadrant id where this robot is located."
  [robot grid-spec]
  (cond-> robot
    (q1? grid-spec robot) (assoc :quadrant 1)
    (q2? grid-spec robot) (assoc :quadrant 2)
    (q3? grid-spec robot) (assoc :quadrant 3)
    (q4? grid-spec robot) (assoc :quadrant 4)))


(defn solution-1 [s col-count row-count sec-count]
  (let [grid-specif (grid-spec col-count row-count)]
    (->> s
         parse
         (map #(move-robot % col-count row-count sec-count))
         (map #(assign-by-quadrant % grid-specif))
         (filter :quadrant)
         (group-by :quadrant)
         (map #(count (second %)))
         (reduce *))))

(comment
  (solution-1 sample-input 11 7 100)
  ;; => 12 ... good
  (solution-1 puzzle-input 101 103 100)
  ;; =>  224554908 ⭐
  ;;
  )

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at a certain step (after a certain amount of seconds) a Christmas tree is drawn
;; by robots !! 😮

;; what ? 

;; What does a christmas tree look like ? 

;; ....X....
;; ...XXX...
;; ..XXXXX..
;; .XXXXXXX.
;; ....X....
;; ....X....

;; something like that ? 
;; Let's assume that if a christams tree is drawn in the grid then we should have
;; 'several' robots aligned. It can be veritcally (the trunc) or horizontaly (the widest side)
;; 
;; We could then, after each move (each second) detect if such case occured
;; This is VERY fuzzy ... but maybe it will work.

;; One potential issue is time processing. With a 103x107 grid it may take a while

;; Given a seq of robots, find consecutive robots

(comment

  ;; move once
  (defn move-one-sec [robots]
    (map #(move-robot % 11 7 1) robots))

  (last (take 2000 (iterate move-one-sec (parse sample-input))))
  ;; stack overflow


  (loop [robots (parse sample-input)
         sec   0]
    (if (= 4000 sec)
      robots
      (recur (move-one-sec robots) (inc sec))))
  ;; stack overflow

  (reduce (fn [robots sec]
            (move-one-sec robots))
          (parse sample-input)
          (take 5000 (iterate inc 0)))
  ;; stack overflow
  ;; 😭


  ;; ok so maybe we will find a christmas tree before reaching the stack overflow error ...
  )

(comment
  ;; how to detect robot aligned given a seq of their positions ?
  ;; They are aligned horizontally if 
  ;; - they all have the same py
  ;; - they have consecutive px

  (defn sort-by-axis [axis-key robots]
    (sort-by axis-key robots))

  (sort-by-axis :px [{:px 2} {:px 4} {:px 1}])

  (defn max-consecutive-by-axis [axis-key robots]
    (reduce (fn [acc robot]
              (if (empty? acc)
                (conj acc robot)
                (if (= (-> acc last axis-key inc) (axis-key robot))
                  (conj acc robot)
                  (vector robot)))) [] robots))

  (max-consecutive-by-axis :px [{:px 2} {:px 3} {:px 4}])

  ;; using partition-by
  (map list [2 3 4] [1 2 3 4])
  (->> (map #(- %1 %2) [2 3 4] [1 2 3 4])
       (partition-by identity))


  ;;
  )

(defn count-consecutive-by-axis
  "Given a seq od robots, returns the max count of consecutive robots on the given `axis-k` where
   `axis-k` is ont of :
   
   - `:px` : for horizontal axis
   - `:py` : for veritcal axis

   Note that `robots` must be have the same (complement `axis-k`) value.
   "
  [axis-k robots]
  (let [count-consec (->> robots
                          (sort-by axis-k)
                          (map :px)
                          ((juxt  next identity))
                          (apply map -)
                          (partition-by identity)
                          (filter #(= 1 (first %)))
                          (map count))]
    (if (seq count-consec)
      (->> count-consec
           (apply max)
           inc)
      0)))

(comment

  (partition-by identity [1 2 3 1 1])
  (count-consecutive-by-axis :px [{:px 2} {:px 1} {:px 5} {:px 6} {:px 7} {:px 4}])

  (count-consecutive-by-axis :px [{:px 2} {:px 4} {:px 30} {:px 33} {:px 36}])
  (apply max '())
  ;;
  )

(defn count-max-horizontal
  "Given a seq of `robots`, returns the max number of horizontally aligned robots."
  [robots]
  (->> robots
       (group-by :py)
       (map (fn [[_py y-robots]]
              (count-consecutive-by-axis :px y-robots)))
       (apply max)))

(comment
  (count-max-horizontal [{:px 2 :py 1} {:px 4 :py 1} {:px 30 :py 3} {:px 33 :py 3} {:px 36 :py 3}])
  (count-max-horizontal [{:px 2 :py 1} {:px 3 :py 2} {:px 30 :py 3} {:px 33 :py 3} {:px 36 :py 3}])
  ;;
  )

(comment
  (defn move-one-sec [robots]
    (map #(move-robot % 103 101 1) robots)
    #_(map #(move-robot % 11 7 1) robots))

  (last (take 2000 (iterate move-one-sec (parse puzzle-input))))

  (->> (parse puzzle-input)
       (iterate move-one-sec)
       (map count-max-horizontal)
       (take 10000)
       (apply max))
  ;;=> 5



  ;;
  )