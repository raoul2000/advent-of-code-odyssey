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

(defn move-x [{:keys [px vx] :as _robot} col-count sec-count]
  (let [max-x (dec col-count)
        dx    (+ px (* sec-count vx))]
    (cond
      (< -1 dx col-count) dx
      (> dx max-x)        (mod dx col-count)
      :else :boo)))


(comment


  (move-x {:px 0 :vx 5} 6 1)
  (move-x {:px 0 :vx 6} 6 1)
  (dec (mod 8 6))
  (mod 14 6)
  ;;
  )
