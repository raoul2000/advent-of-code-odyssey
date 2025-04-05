(ns day-14
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
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

(comment
  (move {:px 0 :vx 10} 6 2)
  (move {:px 0 :vx 6} 6 1)
  ;;
  )

(comment
  ;; dealing with negative velocity
  (def r1 {:px 2 :vx -3})
  (def max-x 5)

  ;; interpolate coordinates ot get a positive velocity
  ;; For X axis, left->right is replaced by right->left
  (def r1-i (if (neg-int? (:vx r1))
              (-> r1
                  (update :vx abs)
                  (update :px #(- max-x %)))
              r1))

  (def result-i (move-x r1-i (inc max-x) 1))
  (def result (- max-x result-i))


  ;;
  )

(defn move-x [{:keys [px vx] :as robot} col-count sec-count]
  (let [r (if (neg? vx)
            {:vx (abs vx), :px (- (dec col-count) px)}
            robot)
        new-pos (move r col-count sec-count)]
    (if (neg? vx)
      (- (dec col-count) new-pos)
      new-pos)))

(comment
  (move-x {:px 1, :vx -3} 6 1))

(defn apply-positive-velocity
  "Compute the new coordinate given a **positive velocity** value."
  [coord positive-velocity axis-size sec-count]
  (let [max-coord   (dec axis-size)
        new-coord   (+ coord (* sec-count positive-velocity))]
    (cond
      (< -1 new-coord axis-size)  new-coord
      (> new-coord max-coord)    (mod new-coord axis-size))))

(comment
  (apply-positive-velocity 0 1 2 1)
  (apply-positive-velocity 0 1 2 2)
  (neg? 0)
  ;;
  )

(defn move-on-axis [coord v axis-size sec-count]
  (let [interpol-coord (cond->> coord
                         (neg? v) (- (dec axis-size)))
        new-coord      (apply-positive-velocity  interpol-coord
                                                 (abs v)
                                                 axis-size
                                                 sec-count)]
    (cond->> new-coord
      (neg? v)  (- (dec axis-size)))))

(comment
  (move-on-axis  0 1 2 2)
  (move-on-axis  0 1 6 1)

  (cond-> -1
    (neg? 1) abs)
  ;;
  )






#_(defn move-robot [robot col-count row-count sec-count]
    (-> robot
        (move-on-single-axis col-count sec-count)
        (move-on-single-axis row-count sec-count)))
