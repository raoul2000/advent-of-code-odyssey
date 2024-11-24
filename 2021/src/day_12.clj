(ns day-12
  (:require [clojure.string :as str]))

;; part 1 =============================================
;; NOTE : the graph is NOT oriented. "A-c" means from "A"
;; it's possible to go to "c" and from "c" got to "A"

(def test-data "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
")

;; graph structure : map
;; {
;;   start [dc ah]
;;   dc    [XB ah]
;;   ah    []
;;   XB    [fi]
;;   fi    [end yw]
;;   etc...
;; }

(defn update-graph [k v m]
  (if (get m k)
    (update m k #(conj % v))
    (assoc m k [v])))

(defn parse-data
  "Returns a map where k is the start cave and v a vector of all cave
   that can be reached from *start*"
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #"-"))
       (reduce (fn [m [start end]]
                 (->> (update-graph start end m)
                      (update-graph end start)))
               {})))

(defn big-cave?
  "Returns TRUE if s is the name of a big cave, FALSE otherwise"
  [s]
  (every? #(Character/isUpperCase %) s))

(defn not-visited?
  "Returns TRUE if *steps-xs* does not contain the step *cave*"
  [cave steps-xs]
  (not (some #{cave} steps-xs)))

(defn add-next-step
  "Given the graph map and a list of steps, returns all paths that cna be built
   by adding an additional step to a new cave"
  [graph-m steps-xs]
  (when-let [next-xs (get graph-m (last steps-xs))]
    (->> (map #(if (or (big-cave? %)
                       (not-visited? % steps-xs))
                 (conj steps-xs %)
                 (conj steps-xs "skip"))
              next-xs)
         (remove nil?))))

(comment
  (add-next-step  (parse-data test-data) ["start"])
  ;; => (["start" "dc"] ["start" "ah"])

  (add-next-step (parse-data test-data) ["start" "dc"])
  (add-next-step (parse-data test-data) ["dc" "start"])
  ;; => (["dc" "start" "skip"] ["dc" "start" "ah"])
  )

(defn explore [m]
  (let [f (partial add-next-step (:graph m))]
    (->> (map f (:steps m))
         (reduce #(into %1 %2) [])
         (reduce (fn [m2 s2]
                   (cond
                     (= "end" (last s2))     (update m2 :paths #(into % [s2]))
                     (not= "skip" (last s2)) (update m2 :steps #(into % [s2]))
                     :else m2))
                 (assoc m :steps [])))))

(defn solve-part-1 [s]
  (->> (iterate explore {:graph (parse-data s)
                         :steps [["start"]]
                         :paths []})
       (take-while #(seq (:steps %)))
       ((comp count :paths explore last))))

(comment
  (solve-part-1 test-data)
  ;; => 19

  (solve-part-1 (slurp "./resources/puzzle_12.txt"))
  ;; => 4241
  )

;; part 2 ================================================
;; it is not allowed to visite one small cave once per path
;; a cave can be added as step if :
;; - it is not "start"
;; AND
;; - it is a big cave
;;  OR ( it has never been visited
;;      OR a small cave have never been visited twice)

(defn small-cave-not-visited-twice? [steps-xs]
  (->> (frequencies steps-xs)
       (filter #(= 2 (last %)))
       (filter #(not (big-cave? (first %))))
       (empty?)))

(defn select-cave? [cave steps-xs]
  (and (not= "start" cave)
       (or (big-cave? cave)
           (or (not-visited? cave steps-xs)
               (small-cave-not-visited-twice? steps-xs)))))

(comment
  (small-cave-not-visited-twice? ["b" "c" "d" "a"])
  (select-cave? "a" ["b" "c" "d"])
  (select-cave? "a" ["b" "c" "d" "a"])
  (select-cave? "a" ["b" "c" "d" "a" "a"])
  (select-cave? "A" ["b" "c" "d" "a" "a"])
  (select-cave? "A" ["b" "c" "d" "a" "a" "A"])
  ;;
  )

(defn add-next-step-2
  "Given the graph map and a list of steps, returns all paths that cna be built
   by adding an additional step to a new cave"
  [graph-m steps-xs]
  (when-let [next-xs (get graph-m (last steps-xs))]
    (->> (map #(if (select-cave? % steps-xs)
                 (conj steps-xs %)
                 (conj steps-xs "skip"))
              next-xs)
         (remove nil?))))

(defn explore-2 [m]
  (let [f (partial add-next-step-2 (:graph m))]
    (->> (map f (:steps m))
         (reduce #(into %1 %2) [])
         (reduce (fn [m2 s2]
                   (cond
                     (= "end" (last s2))     (update m2 :paths #(into % [s2]))
                     (not= "skip" (last s2)) (update m2 :steps #(into % [s2]))
                     :else m2))
                 (assoc m :steps [])))))

(defn solve-part-2 [s]
  (->> (iterate explore-2 {:graph (parse-data s)
                           :steps [["start"]]
                           :paths []})
       (take-while #(seq (:steps %)))
       ((comp count :paths explore last))))

(comment
  (solve-part-2 test-data)
  ;; => 103

  (solve-part-2 (slurp "./resources/puzzle_12.txt"))
  ;; => 122134
  )