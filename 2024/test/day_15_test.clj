(ns day-15-test
  (:require [clojure.test :refer [testing deftest is are]]
            [clojure.set :as st]
            [day-15 :as d15]))


((deftest apply-move-on-tiles-test
   (testing "Moving titles on robot move"
     (is (= '(\. \@)
            (d15/apply-move-on-tiles [\@ \.]))
         "switch when 2 tiles with space")

     (is (= '(\@ \O)
            (d15/apply-move-on-tiles [\@ \O]))
         "don't cvhange when no space")

     (is (= '(\. \@ \O)
            (d15/apply-move-on-tiles [\@ \. \O])))

     (is (= '(\. \@ \. \O)
            (d15/apply-move-on-tiles [\@ \. \. \O])))

     (is (= '(\. \@ \O \O)
            (d15/apply-move-on-tiles [\@ \. \O \O])))

     (is (= '(\. \@ \O \O)
            (d15/apply-move-on-tiles [\@ \O \O \.]))))))

(deftest vector-positions-test
  (let [grid (:grid (d15/parse-input d15/sample-input))]
    (testing "extract vector position"
      (is (= '([1 1] [1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [1 8])
             (d15/vector-positions [1 1] grid d15/move-down-char)))

      (is (= '([8 1])
             (d15/vector-positions [8 1] grid d15/move-right-char))
          "robot at limit right")

      (is (= '([1 8])
             (d15/vector-positions [1 8] grid d15/move-down-char))
          "robot at limit bottom")

      (is (= '([1 1])
             (d15/vector-positions [1 1] grid d15/move-left-char))
          "robot at limit left")

      (is (= '([1 1])
             (d15/vector-positions [1 1] grid d15/move-up-char))
          "robot at limit up")))

  (let [grid2 (-> ;; move robot position down
               (:grid (d15/parse-input d15/sample-input))
               (d15/set-at-pos  [4 5] d15/robot-char)
               (d15/set-at-pos  [4 4] d15/space-char))]
    (is (= '([4 5] [3 5])
           (d15/vector-positions [4 5] grid2 d15/move-left-char))
        "stops on border char")))

(deftest expand-grid-test
  (testing "expand single line"
    (is (= [\# \#]
           (d15/expand-line [\#])))
    (is (= [\# \# \# \#]
           (d15/expand-line [\# \#])))
    (is (= [\[ \]]
           (d15/expand-line [\O])))
    (is (= [\# \# \[ \] \# \#]
           (d15/expand-line [\# \O \#])))
    (is (= [\# \# \@ \. \# \#]
           (d15/expand-line [\# \@ \#]))))


  (testing "Expand grid"
    (is (= [[\# \# \# \# \# \#]
            [\# \# \. \. \# \#]
            [\# \# \# \# \# \#]]

           (d15/expand-grid [[\# \# \#]
                             [\# \. \#]
                             [\# \# \#]]))
        "expand empty small grid")

    (is (= [[\# \# \# \# \# \# \# \#]
            [\# \# \. \. \[ \] \# \#]
            [\# \# \# \# \# \# \# \#]]

           (d15/expand-grid [[\# \# \# \#]
                             [\# \. \O \#]
                             [\# \# \# \#]]))
        "expand space and box")

    (is (= [[\# \# \# \# \# \# \# \#]
            [\# \# \@ \. \# \# \# \#]
            [\# \# \# \# \# \# \# \#]]

           (d15/expand-grid [[\# \# \# \#]
                             [\# \@ \# \#]
                             [\# \# \# \#]]))
        "expand robot and space")))

(deftest move-horizontal-test
  (testing "update a single line on robot horizontal move"
    (is (= [\# \# \. \@ \[ \] \#]
           (d15/update-line-on-horizontal-move [\# \# \@ \[ \] \. \#] d15/move-right-char))
        "move when there is space on right")
    (is (= [\# \# \@ \[ \] \# \#]
           (d15/update-line-on-horizontal-move [\# \# \@ \[ \] \# \#] d15/move-right-char))
        "no change when no space on right")
    (is (= [\# \# \. \@ \[ \] \. \#]
           (d15/update-line-on-horizontal-move [\# \# \@ \. \[ \] \. \#] d15/move-right-char))
        "move when there is space just after robot "))


  #_(testing "moving the robot left or right"
      (is (= [[\# \# \# \# \# \# \# \#]
              [\# \# \. \@ \[ \] \# \#]
              [\# \# \# \# \# \# \# \#]]

             (d15/move-horizontal [[\# \# \# \# \# \# \# \#]
                                   [\# \# \@ \. \[ \] \# \#]
                                   [\# \# \# \# \# \# \# \#]]
                                  d15/move-right-char))
          "moving right")))

