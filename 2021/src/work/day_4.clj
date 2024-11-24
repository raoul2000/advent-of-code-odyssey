(ns work.day-4
  (:require [clojure.string :as str]))



(def test-data "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

;; part 1 ========

(def data (->> test-data;;(slurp "./resources/puzzle_4.txt")
               (str/split-lines)))

(def  draw-num (->> (str/split (first data) #",")
                    (map #(Integer/parseInt % 10))))

(defn numbers->int-coll [s]
  (->> (str/split s #" ")
       (remove str/blank?)
       (map #(Integer/parseInt % 10))))

(numbers->int-coll " 12 65 22 11 26 55 98 78")

(def boards (->> (rest data)
                 (reduce (fn [result line]
                           (if (str/blank? line)
                             (str result "\n")
                             (str result " " line))) "")
                 (str/split-lines)
                 (remove str/blank?)
                 (map numbers->int-coll)
                 (map #(partition 5 %))))
boards




(def b [1 2 3
        4 5 6
        7 8 9

        11 22 33
        44 55 66
        77 88 99

        111 222 333
        444 555 666
        777 888 999])
(defn str->int [s]
  (Integer/parseInt s))

;; read sample data and returns a representation of all boards
;; as a seq of int
(defn read-boards [s]
  (->> (str/split-lines s)
       rest
       (reduce (fn [result s]
                 (into result (->> (str/split s #" ")
                                   (remove str/blank?)
                                   (map str->int)))) [])))
(def d (read-boards test-data))

;; board 1
(partition 3 3 b)
(take 5 (partition 5 d))
;; mark 66
(map #(if (= 66 %) \x %) b)
;; board 1 
;; line 1
(take 3 b)
(take 5 d)
;; col 1
(take 3 (take-nth 3 b))
(take 5 (take-nth 5 d))

;; line 2
(take 3 (drop 3 b))
(take 5 (drop 5 d))
;; col 2
(take 3 (take-nth 3 (drop 1 b)))
(take 5 (take-nth 5 (drop 1 d)))

;; line 3
(take 3 (drop (* 3 2) b))
;; col 3
(take 3 (take-nth 3 (drop 2 b)))

;; board 2
;; line 1
(take 3 (drop (* 9 1) b))
;; col 1
(take 3 (take-nth 3 (drop (* 9 1) b)))

;; line 2
(take 3 (drop (+ (* 9 1) (* 3 1)) b))
;; col 2
(take 3 (take-nth 3 (drop (+ (* 9 1) 1) b)))

;; line 3
(take 3 (drop (+ (* 9 1) (* 3 2)) b))
;; col 3
(take 3 (take-nth 3 (drop (+ (* 9 1) 2) b)))

;; board 3
;; line 1
(take 3 (drop (+ (* 9 2) (* 3 0)) b))
;; col 1
(take 3 (take-nth 3 (drop (+ (* 9 2) (* 1 0)) b)))

;; line 2
(take 3 (drop (+ (* 9 2) (* 3 1)) b))
;; col 2
(take 3 (take-nth 3 (drop (+ (* 9 2) (* 1 1)) b)))

;; line 3
(take 3 (drop (+ (* 9 2) (* 3 2)) b))
;; col 3
(take 3 (take-nth 3 (drop (+ (* 9 2) (* 1 2)) b)))

(def board-width 5)
(def board-height 5)

(defn read-board-line [boards board-idx line-idx]
  (take 3 (drop (+ (* 9 board-idx) (* 3 line-idx)) boards)))

(read-board-line b 0 0)
(read-board-line b 2 2)

(defn read-board-line-5 [boards board-idx line-idx]
  (take 5 (drop (+ (* 25 board-idx) (* 5 line-idx)) boards)))

(read-board-line-5 d 0 0)
(read-board-line-5 d 2 2)
(read-board-line-5 d 2 4)


(defn read-board-col [boards board-idx col-idx]
  (take 3 (take-nth 3 (drop (+ (* 9 board-idx) (* 1 col-idx)) boards))))

(read-board-col b 0 0)
(read-board-col b 2 2)


(defn read-board-col-5 [boards board-idx col-idx]
  (take 5 (take-nth 5 (drop (+ (* 25 board-idx) (* 1 col-idx)) boards))))

(read-board-col-5 d 0 0)
(read-board-col-5 d 0 1)
(read-board-col-5 d 2 0)
(read-board-col-5 d 2 4)

(defn mark-draw [boards draw]
  (map #(if (= draw %) "x" %) boards))

(mark-draw b 1)
(mark-draw d 11)

(defn read-board
  "Returns a seq of values read from the lines in the 
   board given its index in the list of boards"
  [boards board-idx]
  (mapcat #(read-board-line-5 boards board-idx %) (range 0 5)))

(read-board d 0)
(read-board d 1)
(read-board d 2)



(defn read-all-cols
  "Returns a seq of values read from the columns in the 
   board given its index in the list of boards"
  [boards board-idx]
  (mapcat #(read-board-col-5 boards board-idx %) (range 0 5)))

(read-all-cols d 0)
(read-all-cols d 1)
(read-all-cols d 2)

;; ---------

(defn split-by-board [boards]
  (partition 25 boards))

(split-by-board d)

(defn board-lines
  "Returns a seq of lines where each line is a seq of numbers."
  [board]
  (partition 5 board))

(board-lines '(22 13 17 11 0 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19))

(defn board-cols
  "Returns a seq of cols where each col is a seq of numbers."
  [board]
  (loop [nums board
         cols []]
    (if (= 20 (count nums))
      cols
      (recur (rest nums)
             (conj cols (take-nth 5 nums))))))

(board-cols '(22 13 17 11 0 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19))


(defn winner?
  "Returns true if xs has 5 consecutive marks"
  [xs-num]
  (loop [xs (partition 5 xs-num)]
    (cond
      (empty? (first xs)) false
      (= ["x" "x" "x" "x" "x"] (first xs)) true
      :else (recur (rest xs)))))

(winner? ["x" "x" "x" "x" "x" 1 2 3 4 5])
(winner? [1 "x" "x" "x" "x" 1 2 3 4 5])
(winner? [1 2 3 4 5 "x" "x" "x" "x" "x"])
(winner? (read-board b 0))

(defn win? [xs]
  (=  ["x" "x" "x" "x" "x"] xs))

(defn win-by-lines [board]
  (->> (board-lines board)
       (filter win?)
       seq))

(win-by-lines '(22 13 17 11 0 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19))
(win-by-lines '("x" "x" "x" "x" "x" 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19))
(win-by-lines '(1 "x" "x" "x" "x" "x"  2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19))

(defn bingo? [xs]
  (->> (remove neg? xs)
       count
       zero?))

(bingo? [-1 -2 -5])
(bingo? [-1 2 -5])

(defn bingo?-2 [xs]
  (= [\x \x \x \x \x] xs))

(defn winner-board [board]
  (->> (concat (board-lines board) (board-cols board))
       (filter bingo?)
       seq))

(winner-board '(22 13 17 11 0 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19))
(winner-board '(-1 -1 -1 -1 -1 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19))
(winner-board '(22 -1 17 11 0
                   8  -1 23 4 24
                   21 -1  14 16 7
                   6  -1  3  18 5
                   1  -1  20 15 19))

(map winner-board (split-by-board '(22 13 17 11 0 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19
                                       "x" "x" "x" "x" "x" 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19)))


(defn mark-draw [boards num]
  (map #(if (= % num) -1  %) boards)
  ;;(map #(if (= % num) \x  %) boards)
  )

(mark-draw '(22 13 17 11 0 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19) 7)

(defn find-winner-board [boards]
  (loop [[board & remaining] (split-by-board boards)]
    (cond
      (empty? board)   nil
      (winner-board board) board
      :else (recur remaining))))

(find-winner-board d)
(find-winner-board '(22 13 17 11 0 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19
                        -1 -1 -1 -1 -1 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19))


(defn compute-score [board draw]
  (->> (filter pos? board)
       (apply +)
       (* draw)))

(compute-score [-1 -2 2 3 5] 5)

(defn play-bingo [boards [this-draw & remaining-draw]]
  (when this-draw
    (let [boards-played (mark-draw boards this-draw)
          win-board (find-winner-board boards-played)]
      (if win-board
        (compute-score win-board this-draw)
        (recur boards-played
               remaining-draw)))))

(play-bingo d [1 2 3])
(play-bingo d [22 13 17 0 11  ])
(play-bingo d draw-num)



