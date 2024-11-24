(ns day-1
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2023/day/1

(def sample-input "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(comment
  ;; how about using regex ?
  (re-matches #"[a-z]*([0-9]).*([0-9])[a-z]*" "1ab2") ;; ok
  (re-matches #"[a-z]*([0-9]).*([0-9])[a-z]*" "12") ;; ok
  (re-matches #"[a-z]*([0-9]).*([0-9])[a-z]*" "a1b") ;; ko 🤔 

  ;; surely by searching a little bit we could define 
  ;; a suitable regexp.

  ;; let's explore some other option
  ;; if we try with seq :

  (drop-while #(Character/isLetter %1) (seq "abc1cvb"))
  (drop-while #(Character/isLetter %1) (reverse (seq "abc1cvb")))

  (defn take-first-digit [^String calibration]
    (->> (seq calibration)
         (drop-while #(Character/isLetter %))
         (first)))

  (take-first-digit "az12")
  (take-first-digit "7az12")

  (defn take-last-digit [^String calibration]
    (->> (seq calibration)
         reverse
         (drop-while #(Character/isLetter %))
         (first)))

  (take-last-digit "az2r5")
  (take-last-digit "az2r")

  (defn calibration-value [calibration-line]
    (->> (str (take-first-digit calibration-line) (take-last-digit calibration-line))
         (Integer/parseInt)))

  (calibration-value "a1f3")
  (calibration-value "a1f")


  (reduce + (map calibration-value (s/split-lines sample-input)))
  ;; => 142 

  ;; sample input gives correct result
  ;; now try with uzzle input

  (->> (slurp "resources/day_1.txt")
       (s/split-lines)
       (map calibration-value)
       (reduce +))

  ;; => 54388 !! ⭐ .. first start of 2023 
  ;;
  )

;; ok we have a first solution? let's write it down

(defn take-first-digit [^String calibration]
  (->> (seq calibration)
       (drop-while #(Character/isLetter %))
       (first)))

(defn take-last-digit [^String calibration]
  (->> (seq calibration)
       reverse
       (drop-while #(Character/isLetter %))
       (first)))

(defn calibration-value [calibration-line]
  (->> (str (take-first-digit calibration-line) (take-last-digit calibration-line))
       (Integer/parseInt)))

(defn solution-part-1 [calibration-lines]
  (reduce + (map calibration-value calibration-lines)))

(solution-part-1 (s/split-lines (slurp "resources/day_1.txt")))
;; => 54388

;; let's go back to the Regex solution
(comment
  (re-matches #".*(\d).*" "1ze3")
  (re-seq #"\d" "1er3")
  ;; re-seq returns what we want : the list of matching substrings

  ;; using re-seq with first and last, we have a shorter solution
  (reduce + (map (fn [calibration-line]
                   (let [digits (re-seq #"\d" calibration-line)]
                     (Integer/parseInt (str (first digits) (last digits))))) (s/split-lines sample-input)))
  ;;
  )

;; let's write this alternate solution to part 1
(defn solution-part-1-alt [calibration-lines]
  (reduce + (map (fn [calibration-line]
                   (let [digits (re-seq #"\d" calibration-line)]
                     (Integer/parseInt (str (first digits) (last digits))))) calibration-lines)))

(solution-part-1-alt (s/split-lines (slurp "resources/day_1.txt")))
;; => 54388




;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; digit may be spelled in letters !! and they should be included 
;; in the computation

(def sample-input-2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

;; It seems that the alternate solution based on regexp is more convinient
;; to solve this problem.

(comment
  (def re #"\d|one|two|three|four|five|six|seven|eight|nine")

  (map #(re-seq re %1) (s/split-lines sample-input-2))

  ;; we will have to turn spelled digits into actual character digit
  ;; and then merge them together for each line, before sum all value lines

  ;; let's use a map
  (def spelled-digits {"one"     "1"
                       "two"     "2"
                       "three"   "3"
                       "four"    "4"
                       "five"    "5"
                       "six"     "6"
                       "seven"   "7"
                       "eight"   "8"
                       "nine"    "9"})

  (get spelled-digits "one")
  (get spelled-digits \2 \2)

  ;; create a function that handle both spelled and 
  ;; string digits: if input str is not in the spelled-digit map
  ;; then we are dealing with a digit as string that must be returned
  ;; with no change.

  (defn ->digit-str [s]
    (get spelled-digits s s))

  (->digit-str "one")
  (->digit-str "2")

  ;; basically applying same as part 1 but with additional
  ;; step to convert spelled digit into string digit

  ;; trying with smaple inputs
  (reduce + (map (fn [calibration-line]
                   (let [spelled-or-num-digits (re-seq re calibration-line)]
                     (Integer/parseInt (str (->digit-str (first spelled-or-num-digits))
                                            (->digit-str (last spelled-or-num-digits))))))

                 (s/split-lines sample-input-2)))
  ;; => 281 🥇


  ;; trying with puzzle inputs ...
  (reduce + (map (fn [calibration-line]
                   (let [spelled-or-num-digits (re-seq re calibration-line)]
                     (Integer/parseInt (str (->digit-str (first spelled-or-num-digits))
                                            (->digit-str (last spelled-or-num-digits))))))

                 (s/split-lines (slurp "resources/day_1.txt"))))
  ;; => 53519
  ;; this is NOT the correct answer 😪

  ;; hum ... ok, it is time to use the 'test' namespace
  ;; first, extract required function from this rich comment 
  )

(def re #"\d|one|two|three|four|five|six|seven|eight|nine")
(def spelled-digits {"one"     "1"
                     "two"     "2"
                     "three"   "3"
                     "four"    "4"
                     "five"    "5"
                     "six"     "6"
                     "seven"   "7"
                     "eight"   "8"
                     "nine"    "9"})

(defn ->digit-str [s]
  (get spelled-digits s s))

;; extract this mapper so to be able to test it
(defn calibration-value [calibration-line]
  (let [spelled-or-num-digits (re-seq re calibration-line)]
    (Integer/parseInt (str (->digit-str (first spelled-or-num-digits))
                           (->digit-str (last spelled-or-num-digits))))))

(defn solution-part-2 [calibration-lines]
  (reduce + (map calibration-value calibration-lines)))

(solution-part-2 (s/split-lines (slurp "resources/day_1.txt")))

;; Ooook. So after some search and with the help of reddit (https://www.reddit.com/r/adventofcode/comments/188wjj8/2023_day_1_did_not_see_this_coming/)
;; I realized that this case causes issue:
;; ".....oneight..."
;; "One" and "Eight" overlap, and with the current regexp, the winner is the
;; first match = "one". In fact the last match is "eight".
;; There could be other overlapping spelled values in the input data (although I didn't found
;; any other that this 'oneight)
;; We may have to review the regexp then ...

;; After some search, it seems that lookahread assertion should be the solution
;; see https://www.regular-expressions.info/lookaround.html

;; let's try to redefine our regex


(def re-2 #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))")

(comment
  (re-seq re-2 "abconeightsdfs")
  (re-seq re-2 "1a2bconeightsdfsixs")
  ;; => (["" "1"] ["" "2"] ["" "one"] ["" "eight"] ["" "six"])
  ;; the return seq contains items which are 2 items arrays
  ;; the existing function should be adapted to this
  )

(defn calibration-value-2 [calibration-line]
  (let [spelled-or-num-digits (map second (re-seq re-2 calibration-line))]
    (Integer/parseInt (str (->digit-str (first spelled-or-num-digits))
                           (->digit-str (last spelled-or-num-digits))))))

(defn solution-part-2-2 [calibration-lines]
  (reduce + (map calibration-value-2 calibration-lines)))

;; let's try with sample inputs

(solution-part-2-2 (s/split-lines sample-input-2))
;; => 281 👍 good. 
;; And now the puzzle inputs ....

(solution-part-2-2 (s/split-lines (slurp "resources/day_1.txt")))
;; => 53515 🎉

;; Yess !! we have our first ⭐
;; it was not as easy as expected for the first day, but we did it. 
;; let's see what is going to happen for Day 2 !

;; ciao







