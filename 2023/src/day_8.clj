(ns day-8
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/8

;;;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have a network made of nodes. Each node has a name and two successors :
;; - a left 
;; - a right 

;; We start at node AAA and must navigate the network until we reach node ZZZ.
;; Navigation is done following instructions provided in the form of a string
;; made of L (for left) and R (for right) moves.

(def sample-input "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

;; First I thought to use zipper. Then I realized that the network can have loops so
;; it is not suitable to be represented as a zipper.
;; Maybe just a map will do

(def network-example {"AAA" ["BBB" "CCC"]
                      "BBB" ["DDD" "EEE"]})

(comment
  ;; The sequence of instructions is a seq of chars that repeat itself for ever.
  ;; To implement this, we can use cycle

  ;; For example : 
  (take 5 (cycle [\L \R]))
  (rest (take 5 (cycle [\L \R])))
  ;;


  ;; Now we can create a state data with this shape : 
  (def state-example {:network       network-example
                      :current-node ["AAA" ["BBB" "CCC"]]
                      :step-count   0})

  ;; Creating the initial state includes parsing input data
  (def initial (let [[moves _ & node-lines] (s/split-lines sample-input)]
                 {:network      (reduce (fn [acc line]
                                          (let [[[_ node-id left-id right-id]] (re-seq #"(\w\w\w) = \((\w\w\w), (\w\w\w)\)" line)]
                                            (assoc acc node-id (vector left-id right-id)))) {} node-lines)
                  :moves        (seq moves)
                  :current-node-id "AAA"
                  :step-count   0}))


  ;; Reduction of the initial state 

  (reduce (fn [state move]
            (tap> state)
            (if (= "ZZZ" (:current-node-id state))
              (reduced (:step-count state))
              (-> state
                  (update :step-count inc)
                  (update :current-node-id (fn [cur-node-id]
                                             (let [[left-id right-id] (get (:network state) cur-node-id)]
                                               (if (= \L move) left-id right-id)))))))
          initial
          '(\R \L \R \L \R \L \R \L \R \L)
          #_(take 10 (cycle (:moves initial))))

  ;; Let's create nice functions
  )

(defn create-network [node-lines]
  (reduce (fn [acc line]
            (let [[[_ node-id left-id right-id]] (re-seq #"(\w\w\w) = \((\w\w\w), (\w\w\w)\)" line)]
              (assoc acc node-id (vector left-id right-id)))) {} node-lines))

(defn parse-input [input]
  (let [[moves _ & node-lines] (s/split-lines input)]
    {:network         (create-network node-lines)
     :moves           (seq moves)
     :current-node-id "AAA"
     :step-count      0}))

(defn create-navigator [network]
  (fn [move current-node-id]
    (let [[left-id right-id] (get network current-node-id)]
      (if (= \L move) left-id right-id))))

(defn solution-1 [input]
  (let [initial-state (parse-input input)
        navigate      (create-navigator (:network initial-state))]
    (reduce (fn [state move]
              (tap> state)
              (if (= "ZZZ" (:current-node-id state))
                (reduced (:step-count state))
                (-> state
                    (update :step-count       inc)
                    (update :current-node-id  (partial navigate move)))))
            initial-state
            (cycle (:moves initial-state)))))

(comment

  (solution-1 sample-input)
  ;; still ok with sample input

  ;; Trying with puzzle input : 
  (solution-1 (slurp "resources/day_8.txt"))
  ;; => 15517 ⭐ one more star !
  )

;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now the rules change (like always)
;; - there are several starts, all nodes ending by 'A'
;; - there are several ends, nodes ending with 'Z'
;; - navigate at the same time from all start nodes until for each one of them
;;   and end node is reached at the same time.

;; Let's try to reuse function from part 1. Now the initial state changes a little bit : instead of a single
;; value :current-node-id we're dealing with a seq of nodes (whose names all ends with 'A')

;; Consequently, the navigate function must now be applied to all current nodes and not only to one 

;; Another change is the reduced condition. Now all current nodes ids must end with 'Z' for the reduction to
;; be complete.

(def sample-input-2 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(defn select-start-nodes [network]
  (filterv #(s/ends-with? % "A") (keys network)))

(defn parse-input-1 [input]
  (let [[moves _ & node-lines] (s/split-lines input)
        network                (create-network node-lines)]
    {:network          network
     :moves            (seq moves)
     :current-nodes-id (select-start-nodes network)
     :step-count       0}))

(comment
  (parse-input-1 sample-input-2)
  (parse-input-1 (slurp "resources/day_8.txt"))
  ;;
  )

(defn reached-the-end [node-ids]
  (tap> node-ids)
  (not (some #(not (s/ends-with? % "Z")) node-ids)))

(comment
  (reached-the-end ["AZ" "AZ"])
  (reached-the-end ["AZ" "AB"])
  ;;
  )


(defn reached-the-end-1 [node-ids]
  (= (count node-ids)
     (count (filter #(s/ends-with? % "Z") node-ids))))

(comment
  (reached-the-end-1 ["AZ" "AZ"])
  (reached-the-end-1 ["AZ" "AB"])
  ;;
  )


(defn create-navigator-1 [network]
  (fn [move current-node-id-coll]
    (mapv (fn [current-node-id]
            (let [[left-id right-id] (get network current-node-id)]
              (if (= \L move) left-id right-id)))
          current-node-id-coll)))

;; With this, we should be able to rewrite solution

(defn solution-2 [input]
  (let [initial-state (parse-input-1 input)
        navigate      (create-navigator-1 (:network initial-state))]
    (reduce (fn [state move]
              (if (reached-the-end (:current-nodes-id state))
                (reduced (:step-count state))
                (-> state
                    (update :step-count        inc)
                    (update :current-nodes-id  (partial navigate move)))))
            initial-state
            (cycle (:moves initial-state)))))

(comment

  (solution-2 sample-input-2)
  ;; => 6 is the expected result

  ;; Trying with puzzle input : 
  (solution-2 (slurp "resources/day_8.txt"))
  ;; 😱 StackOverflowError

  ;; yeah right, that was too easy to be true !
  ;; After some reading I assume that the stackoverflow exception was caused by the lazyness
  ;; of the map function that gets realized only when consumed.

  ;; read : 
  ;; - https://medium.com/@nikosfertakis/clojure-lazy-evaluation-and-stack-overflow-exceptions-1b8ee732ba0b
  ;; - https://stackoverflow.com/questions/24958907/why-does-reduce-give-a-stackoverflowerror-in-clojure
  ;; - stuart Sierra : https://stuartsierra.com/2015/04/26/clojure-donts-concat

  ;; By replacing map and filter with mapv and filterv we are getting realized vector (i.e. not lazy)
  ;; and the StackOverflow exception goes away.
  ;; ... but ...
  ;; the solution take ages to run. Obviously we must find something else.


  ;; The puzzle input contains 6 initial nodes : 
  (def start-nodes-id (:current-nodes-id  (parse-input-1 (slurp "resources/day_8.txt"))))

  ;;=>  ["DNA" "JVA" "XLA" "DLA" "SHA" "AAA"]

  ;; We will work on each node separately to see if there is a pattern somewhere. First would be 
  ;; to search for loops : what happens after a node has reaches its destination and it performs
  ;; additional move ? where does it leads it to ?

  ;; Let's use our solution from part 1 except that instead of terminating the reduce function when 
  ;; reached end node, we will continue to see if there is a loop

  ;; 🛑 First I must give credit to ;; see https://www.youtube.com/watch?v=xH9LFkYRTD0 to help me with the
  ;; solution.

  ;; I want to check if the steps for each start node is a loop and if this loop, when reaching the end node,
  ;; goes back to the first next node. For this, I create the function below that navigate the newtork for the 
  ;; given starting node but does not stop when reahcing the end node. Instead, it stores some info and continue
  ;; naivgation.
  ;; Once it has reached 4 times the end node, it stops so we can have a look at history collected.

  (defn find-end-node [initial-node-id]
    (let [initial-state (-> (parse-input (slurp "resources/day_8.txt"))
                            (assoc :current-node-id initial-node-id)
                            (assoc :steps           [])
                            (assoc :loop-count      0))
          navigate      (create-navigator (:network initial-state))]
      (reduce (fn [state move]
                (if (>  (:loop-count state) 3)
                  (reduced state)
                  (-> (if (s/ends-with? (:current-node-id state) "Z")
                        (-> state
                            (update :history conj {:steps         (:steps state)
                                                   :last-node-id  (:current-node-id state)
                                                   :next-move     move
                                                   :next-node-id  (navigate move (:current-node-id state))
                                                   :next-node-idx (.indexOf (:steps state) (navigate move (:current-node-id state)))
                                                   :steps-count   (:step-count state)})
                            (assoc  :steps      [])
                            (assoc  :step-count 0)
                            (update :loop-count inc))
                        state)
                      (update :steps            conj (:current-node-id state))
                      (update :step-count       inc)
                      (update :current-node-id  (partial navigate move)))))
              initial-state
              (cycle (:moves initial-state)))))

  ;; We see that for each start node
  ;; - there is indeed a loop
  ;; - the loop does re-start on the first nest node (see :next-node-idx)
  ;; - the number of steps is constant
  ;; 
  ;; let's calculate for all 6 start nodes, the number of steps in their loop
  (tap> (find-end-node "DNA")) ;; 19199
  (tap> (find-end-node "JVA")) ;; 13939
  (tap> (find-end-node "XLA")) ;; 17621
  (tap> (find-end-node "DLA")) ;; 20777
  (tap> (find-end-node "SHA")) ;; 12361
  (tap> (find-end-node "AAA")) ;; 15517

  ;; Then find the lcm for all these values
  ;; using https://www.calculatorsoup.com/calculators/math/lcm.php

  ;; lcm = 14935034899483
  ;; yes ! ⭐

  ;; Ok, this start should actually be given to https://www.youtube.com/@WilliamYFeng for his help
  ;; via this video : https://www.youtube.com/watch?v=xH9LFkYRTD0

  )