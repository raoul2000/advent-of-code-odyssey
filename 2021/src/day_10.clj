(ns day-10
  (:require [clojure.string :as str]))

(def test-data "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(def chunk {\{ \}
            \[ \]
            \( \)
            \< \>})

(def scoring {\) 3
              \] 57
              \} 1197
              \> 25137})


(defn push-stack [m c]  (update m :stack #(conj % c)))
(defn pop-stack  [m]    (update m :stack  pop))
(defn set-error  [m c]  (assoc m :error-char c))
(defn stack-head= [m c] (= c (first (:stack m))))
(def  has-error? :error-char)

(defn analyze-line [s]
  (reduce (fn [result c]
            (if (has-error? result)
              result
              (let [closing-char (get chunk c)]
                (cond
                  closing-char           (push-stack result closing-char)
                  (stack-head= result c) (pop-stack result)
                  :else                  (set-error result c)))))
          {:error-char nil,  :stack '()}
          s))

(comment
  (analyze-line "[{[{({}]{}}([{[{{{}}([]")
  (analyze-line "(([[{{}}]]))"))

(defn solve-part-1 [s]
  (->> (str/split-lines s)
       (map analyze-line)
       (map :error-char)
       (remove nil?)
       (map scoring)
       (apply +)))

(comment
  (solve-part-1 test-data)
  ;; => 26397
  (solve-part-1 (slurp "./resources/puzzle_10.txt"))
  ;; => 399153
  )

;; part 2 ========================

(def scoring-2 {\) 1
                \] 2
                \} 3
                \> 4})

(defn auto-complete-score [xs]
  (reduce (fn [result c]
            (+ (* 5 result) (get scoring-2 c)))
          0
          xs))

(comment
  (auto-complete-score '(\) \} \> \] \} \))))

(defn solve-part-2 [s]
  (let [auto-complete (->> (str/split-lines s)
                           (map analyze-line)
                           (filter (comp nil? :error-char))
                           (map :stack)
                           (remove empty?)
                           (map auto-complete-score)
                           (into [])
                           sort)]
    (nth auto-complete (quot (count auto-complete) 2))))

(comment
  (solve-part-2 test-data)
  ;; => 288957
  (solve-part-2 (slurp "./resources/puzzle_10.txt"))
  ;; => 2995077699
  )