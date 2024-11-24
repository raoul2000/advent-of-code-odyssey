(ns day-5
  (:require [clojure.string :refer [split-lines split]]
            [clojure.set :refer [intersection]]))

;; https://adventofcode.com/2022/day/5

;; are moved one at a time
;; which crate will end up on top of each stack
;;

(def sample "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

;; stacks line
;; ... ... ...\n
;; [Z] [M] [P]
;; stack will be vector where top element is first vector item
;; [G]
;; [T]
;; [Y]
;;
;; => [G T Y]

(defn parse-move
  "Given *s* a move description, returns a coll of 3 int values where
   the first is the count of items to move, the second and thirst the 1 based index
   of the source and target stack."
  [s]
  (->> (re-matches #"move (\d+) from (\d+) to (\d+)"  s)
       (rest)
       (map #(Integer/parseInt % 10))))

(comment
  (parse-move  "move 8 from 1 to 4")
  (parse-move  "move 12 from 8 to 7"))

(defn parse-stack-line [s]
  (map (fn [coll]
         (when-not (= \space (first coll))
           (second coll))) (partition 3 4 s)))

(defn create-stack-coll
  "Given coll of *lines* representing stacks content, returns a vector
   of stacks where the first item is the last inserted item."
  [lines]
  (->> (map parse-stack-line lines)
       (apply map (fn [& args] (apply vector args)))
       (map (partial drop-while nil?))
       (into [])))

(defn apply-single-move [stack-coll [q from to]]
  (let [to-move (take q (stack-coll (dec from)))]
    (-> stack-coll
        (update ,,, (dec from) #(drop q %))
        (update ,,, (dec to)   #(into % to-move)))))

(defn apply-all-moves [stack-coll move-coll]
  (if (empty? move-coll)
    stack-coll
    (recur (apply-single-move stack-coll (first move-coll))
           (rest move-coll))))

(defn solution-1 []
  (let [lines                          (split-lines (slurp "./resources/puzzle_5.txt"))
        [indexed-stack _  moves-lines] (partition-by #(= "" %) lines)
        stack-coll                     (create-stack-coll (butlast indexed-stack))
        move-coll                      (map parse-move moves-lines)]
    (apply-all-moves stack-coll move-coll)))

(comment
  (solution-1)
  ;;
  ;;[(\H \N \M \C)
  ;;(\N \J)
  ;;(\S)
  ;;(\N \V \L \L)
  ;;(\M \D \L \C)
  ;;(\T \D \H \M \G \T \R \B \H \W \L \P \J \Q \T \W \Q \F \V \B \R \M)
  ;;(\L \L \P \H \G \P \D \G \Z \R \F \Q \G \D)
  ;;(\H \C \M)
  ;;(\Q \R)]
  ;;
  ;;=> HNSNMTLHQ
  )

;; part 2 -------------------------------------------------

;; the ability to pick up and move multiple crates at once
;; moved crates stay in the same order
;; After the rearrangement procedure completes, what crate ends up on top of each stack?


(defn apply-single-move-2 [stack-coll [q from to]]
  (let [to-move (take q (stack-coll (dec from)))]
    (-> stack-coll
        (update ,,, (dec from) #(drop q %))
        (update ,,, (dec to)   #(into % (reverse to-move)))))) ;; just reverse

(defn apply-all-moves-2 [stack-coll move-coll]
  (if (empty? move-coll)
    stack-coll
    (recur (apply-single-move-2 stack-coll (first move-coll))
           (rest move-coll))))

(defn solution-2 []
  (let [lines                          (split-lines (slurp "./resources/puzzle_5.txt")
                                                    )
        [indexed-stack _  moves-lines] (partition-by #(= "" %) lines)
        stack-coll                     (create-stack-coll (butlast indexed-stack))
        move-coll                      (map parse-move moves-lines)]
    (apply-all-moves-2 stack-coll move-coll)))

(comment
  
  (solution-2)
  ;;
  ;; [(\R \B \S \M) 
  ;; (\N \L)
  ;;    (\L)
  ;;      (\F \J \D \W)
  ;;      (\D \G \H \G)
  ;;      (\J \N \C \Z \H \P \R \F \G \M \M \R \R \W \H \D \P \D \L \P \Q \C)
  ;;      (\M \V \L \G \T \M \L \B \L \Q \T \N \Q \Q)
  ;;      (\C \H \H)
  ;;      (\T \V)]
  ;;
  ;;=> RNLFDJMCT !!
  ;;
  )