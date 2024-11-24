(ns day-13
  (:require [clojure.string :as str]))

;; part 1 ==========================================

(def test-data "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
")

(defn parse-coords [xs]
  (->> (map #(str/split % #",") xs)
       (map (fn [[x y]]
              [(Integer/parseInt x)
               (Integer/parseInt y)]))))

(defn parse-instructions
  "Convert all instructions in *xs* into a 2 items array:
   
   Example:
   ```
   (parse-instructions [\"fold along y=7\" \"fold along x=2\"])
   => ([\"y\" 7] [\"x\" 2])                                            
   ```"
  [xs]
  (map (fn [s]
         (when-let [[_ axis s] (re-matches #"fold along ([xy])=([0-9]+)" s)]
           [axis (Integer/parseInt s)])) xs))


(comment
  (parse-instructions ["fold along y=7" "fold along x=2"])
  ;;
  )

(defn parse-data [s]
  (let [[coords _ fold-instr] (->> (str/split-lines s)
                                   (partition-by #{""}))]
    [(parse-coords coords)
     (parse-instructions fold-instr)]))

(comment
  (parse-data test-data)
  (parse-data (slurp "./resources/puzzle_13.txt"))
  ;;
  )

(defn paper-size [coords]
  [(inc (apply max (map first coords)))
   (inc (apply max (map last coords)))])

(comment
  (let [[coords _] (parse-data (slurp "./resources/puzzle_13.txt"))]
    (paper-size coords)))

(def empty-point \_)
(def marked-point \X)

(defn create-transparent-paper [col-count line-count]
  (into [] (repeatedly line-count #(into [] (repeat col-count empty-point)))))

(comment
  (create-transparent-paper 5 5)
  (create-transparent-paper 5 8))

(defn mark-points [points paper]
  (reduce (fn [r [x y]]
            (update r y #(update % x (constantly marked-point))))
          paper
          points))

(defn cols->lines
  "Returns a matrix (seq of seq) where the cols in the given matrix
   become lines in the result matrix.
   ```
   (cols->lines [[:a :b]
                 [ 1  2]])
   => ([:a 1] [:b 2])
   ```"
  [xs]
  (apply map vector xs))

(comment
  (cols->lines [[:a :b]
                [1 2]])
  ;;
  )
(defn split-on-fold
  "Returns a seq containing 2 items, result of spliting *xs* in 2 seq 
   before and after item at position fold. Item at position fold ignored.

   ```
   (split-on-fold 2 [1 2 3 4 5])
   => [(1 2) (4 5)]
   ```"
  [fold xs]
  (->> ((juxt take  #(drop (inc %1) %2)) fold xs)
       (map vec)))

(comment
  (split-on-fold 2 [1 2 3 4 5]))

(defn rpad-with-vect [vec-xs len]
  (let [xs-len (count vec-xs)
        vec-len (count (first vec-xs))]
    (if (< len xs-len)
      vec-xs
      (into vec-xs (repeat (- len xs-len)
                           (vec (repeat vec-len nil)))))))

(comment
  (rpad-with-vect [[1 2 :a]
                   [3 4 :b]]
                  3))

(defn fold-xs [fold-index merge-fn xs]
  (let [[l1 l2] (split-on-fold fold-index xs)
        rl1     (vec (reverse l1))
        xs-len  (count xs)]
    (->> (map vector (rpad-with-vect rl1 xs-len) (rpad-with-vect l2 xs-len))
         (map merge-fn)
         (remove (comp nil? first))
         reverse
         vec)))

(defn merge-dots [d1 d2]
  (cond
    (and (nil? d1) d2) d2
    (and (nil? d2) d1) d1
    (= d1 d2)          d1
    :else marked-point))

(defn merge-dot-xs [v]
  (vec
   (map merge-dots (first v) (second v))))

(comment
  (fold-xs 2 merge-dot-xs [[\_ \_]
                           [\X \_]
                           [\X \X]
                           [\X \X]]))

(defn fold-x [x paper]
  (->> (cols->lines paper)
       (fold-xs x merge-dot-xs)
       (cols->lines)))

(defn fold-y [y paper]
  (fold-xs  y merge-dot-xs paper))

(defn solve-part-1 [s fold-instr]
  (let [[coords _]             (parse-data s)
        [col-count line-count] (paper-size coords)]
    (->> (create-transparent-paper col-count line-count)
         (mark-points coords)
         fold-instr
         
         flatten
         frequencies)))

(comment
  (solve-part-1 test-data (partial fold-y 7))
  ;; => 17
  (solve-part-1 (slurp "./resources/puzzle_13.txt") (partial fold-x 655 ))
  ;; => 770
  )

;; part 2 =========================================

(defn draw-paper [coords]
  (let [[col-count line-count] (paper-size coords)]
    (->> (create-transparent-paper col-count line-count)
         (mark-points coords))))

(defn execute-instrucions [paper [axis v]]
  (if (= axis "x")
    (fold-x v paper)
    (fold-y v paper)))

(defn solve-part-2 [s]
  (let [[coords instructions]             (parse-data s)
        paper (draw-paper coords)
        folded-paper (reduce execute-instrucions  paper instructions)]
    (doseq [line folded-paper]
      (println line))))

(comment
  (solve-part-2 test-data)
  ;; [X X X X X]
  ;; [X _ _ _ X]
  ;; [X _ _ _ X]
  ;; [X _ _ _ X]
  ;; [X X X X X]
  ;; [_ _ _ _ _]
  ;; [_ _ _ _ _]

  (solve-part-2 (slurp "./resources/puzzle_13.txt"))
  ;; [X X X X _ X X X _ _ X _ _ X _ X X X X _ X _ _ _ _ X X X _ _ X X X _ _ X X X _ _]
  ;; [X _ _ _ _ X _ _ X _ X _ _ X _ X _ _ _ _ X _ _ _ _ X _ _ X _ X _ _ X _ X _ _ X _]
  ;; [X X X _ _ X _ _ X _ X _ _ X _ X X X _ _ X _ _ _ _ X _ _ X _ X X X _ _ X _ _ X _]
  ;; [X _ _ _ _ X X X _ _ X _ _ X _ X _ _ _ _ X _ _ _ _ X X X _ _ X _ _ X _ X X X _ _]
  ;; [X _ _ _ _ X _ _ _ _ X _ _ X _ X _ _ _ _ X _ _ _ _ X _ _ _ _ X _ _ X _ X _ X _ _]
  ;; [X X X X _ X _ _ _ _ _ X X _ _ X X X X _ X X X X _ X _ _ _ _ X X X _ _ X _ _ X _]  
  )
