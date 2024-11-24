(ns day-4
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

;; boards are represented as a list of int
;; draw num are marked on boards by replacing the existing value with -1

(defn parse-data [s]
  (let [[draw-num-line & board-lines] (str/split-lines s)]
    [;; create draw num seq
     (->> (str/split draw-num-line #",")
          (map #(Integer/parseInt % 10)))

     ;; create boards seq
     (reduce (fn [result s]
               (into result (->> (str/split s #" ")
                                 (remove str/blank?)
                                 (map #(Integer/parseInt %)))))
             []
             board-lines)]))

(defn mark-draw [boards num]
  (map #(if (= % num) -1  %) boards))

(defn bingo?
  "shout \"bino!\" (true) when all num are negative"
  [xs]
  (->> (remove neg? xs)
       count
       zero?))

(defn board-lines
  "Returns a seq of lines where each line is a seq of numbers."
  [board]
  (partition 5 board))

(defn board-cols
  "Returns a seq of cols where each col is a seq of numbers."
  [board]
  (loop [nums board
         cols []]
    (if (= 20 (count nums))
      cols
      (recur (rest nums)
             (conj cols (take-nth 5 nums))))))

(defn winner-board
  "Returns a board if one line or col is bingo !"
  [board]
  (->> (concat (board-lines board) (board-cols board))
       (filter bingo?)
       seq))

(defn find-winner-board [boards]
  (loop [[board & remaining] (partition 25 boards)]
    (cond
      (empty? board)       nil    ;; no more board to test
      (winner-board board) board  ;; winner board found
      :else (recur remaining))))  ;; no winner in this iteration

(defn compute-score [board draw]
  (->> (filter pos? board)
       (apply +)
       (* draw)))

(defn play-bingo [boards [draw & remaining-draws]]
  (when draw
    (let [updated-boards (mark-draw boards draw)
          win-board      (find-winner-board updated-boards)]
      (if win-board
        (compute-score win-board draw)
        (recur updated-boards
               remaining-draws)))))

(let [[draw-num boards] (parse-data test-data)]
  (play-bingo boards draw-num))
;; => 4512

(let [[draw-num boards] (parse-data (slurp "./resources/puzzle_4.txt"))]
  (play-bingo boards draw-num))
;; => 50008

;; part 2 ==============================

;; because after one draw, more than one board may win and thus
;; be removed for the boards list
(defn remove-board [board boards]
  (apply concat (filter #(not= % board) (partition 25 boards))))


(defn update-boards 
  "Remove all winning boards and returns [updated boards, last winning board]"
  [boards last-winner]
  (let [winner-board (find-winner-board boards)]
    (if (not winner-board)
      [boards last-winner]
      (recur (remove-board winner-board boards)
             winner-board))))

(defn play-bingo-3 [boards [draw & remaining-draws] [last-win-board win-draw]]
  (let [[updated-boards win-board] (update-boards (mark-draw boards draw) nil)
        [prec-win-board
         prec-win-draw] (if win-board
                          [win-board draw]
                          [last-win-board win-draw])]
    (if (empty? remaining-draws)
      (compute-score prec-win-board prec-win-draw)
      (recur updated-boards
             remaining-draws
             [prec-win-board, prec-win-draw]))))

(let [[draw-num boards] (parse-data test-data)]
  (play-bingo-3 boards draw-num nil))
;; => 1924

(let [[draw-num boards] (parse-data (slurp "./resources/puzzle_4.txt"))]
  (play-bingo-3 boards draw-num nil))
;; => 17408