(ns day-15-test
  (:require [clojure.test :refer [testing deftest is are]]
            [clojure.zip :as z]
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
        "move when there is space on right (1)")
    (is (= [\# \# \. \@ \[ \] \[ \] \#]
           (d15/update-line-on-horizontal-move [\# \# \@ \[ \] \[ \] \. \#] d15/move-right-char))
        "move when there is space on right (2)")
    (is (= [\# \# \. \@ \[ \] \[ \] \. \#]
           (d15/update-line-on-horizontal-move [\# \# \@ \[ \] \. \[ \] \. \#] d15/move-right-char))
        "move when there is space on right (3)")
    (is (= [\# \# \@ \[ \] \# \#]
           (d15/update-line-on-horizontal-move [\# \# \@ \[ \] \# \#] d15/move-right-char))
        "no change when no space on right")
    (is (= [\# \# \. \@ \[ \] \. \#]
           (d15/update-line-on-horizontal-move [\# \# \@ \. \[ \] \. \#] d15/move-right-char))
        "move when there is space just after robot on right")

    (is (= [\# \# \[ \] \@ \. \#]
           (d15/update-line-on-horizontal-move [\# \# \. \[ \] \@ \#] d15/move-left-char))
        "move when there is space on left")
    (is (= [\# \# \[ \] \[ \] \@ \. \#]
           (d15/update-line-on-horizontal-move [\# \# \. \[ \] \[ \] \@ \#] d15/move-left-char))
        "move when there is space on left")
    (is (= [\# \# \[ \] \@ \# \#]
           (d15/update-line-on-horizontal-move [\# \#  \[ \] \@ \# \#] d15/move-left-char))
        "no change when no space on left")
    (is (= [\# \# \.  \[ \] \@ \. \#]
           (d15/update-line-on-horizontal-move [\# \#  \. \[ \] \. \@ \#] d15/move-left-char))
        "move when there is space just after robot on left"))


  (testing "moving the robot left or right"
    (is (= [[\# \# \# \# \# \# \# \#]
            [\# \# \. \@ \[ \] \# \#]
            [\# \# \# \# \# \# \# \#]]

           (d15/move-horizontal [[\# \# \# \# \# \# \# \#]
                                 [\# \# \@ \. \[ \] \# \#]
                                 [\# \# \# \# \# \# \# \#]]
                                d15/move-right-char))
        "moving right")

    (is (= [[\# \# \# \# \# \# \# \#]
            [\# \# \. \[ \] \@ \. \#]
            [\# \# \# \# \# \# \# \#]]

           (d15/move-horizontal [[\# \# \# \# \# \# \# \#]
                                 [\# \# \. \. \[ \] \@ \#]
                                 [\# \# \# \# \# \# \# \#]]
                                d15/move-left-char))
        "moving left")

    (is (= [[\# \# \# \# \# \# \# \#]
            [\# \# \. \# \[ \] \@ \#]
            [\# \# \# \# \# \# \# \#]]

           (d15/move-horizontal [[\# \# \# \# \# \# \# \#]
                                 [\# \# \. \# \[ \] \@ \#]
                                 [\# \# \# \# \# \# \# \#]]
                                d15/move-left-char))
        "move is not possible (no space)")))

(deftest create-grid-zipper-test
  (testing "grid zipper"
    (let [grid-base [[\# \# \# \# \#]
                     [\# \. \. \. \#]
                     [\# \[ \] \. \#]
                     [\# \. \[ \] \#]
                     [\# \[ \] \. \#]
                     [\# \. \. \. \#]
                     [\# \# \# \# \#]]
          grid       (d15/set-at-pos grid-base [2 5] \@)
          grid-zip   (d15/create-grid-zipper grid d15/move-up-char [2 5])

          grid-1     (d15/set-at-pos grid-base [1 5] \@)
          grid-zip-1 (d15/create-grid-zipper grid-1 d15/move-up-char [1 5])

          grid-2     (d15/set-at-pos grid-base [1 1] \@)
          grid-zip-2 (d15/create-grid-zipper grid-2 d15/move-up-char [1 1])

          grid-3     (d15/set-at-pos grid-base [1 3] \@)
          grid-zip-3 (d15/create-grid-zipper grid-3 d15/move-up-char [1 3])

          grid-4     (d15/set-at-pos grid-base [2 1] \@)
          grid-zip-4 (d15/create-grid-zipper grid-4 d15/move-down-char [2 1])]


      (is (= [[2 5] [2 4] [2 3] [2 2] [1 2] [3 3] [1 4]] (d15/get-connected-tiles grid-zip)))
      (is (= [[2 2] [1 2] [3 3] [1 4]]                   (d15/get-leaves-tiles grid-zip)))

      (is (= [[1 5] [1 4] [2 4] [2 3] [2 2] [1 2] [3 3]] (d15/get-connected-tiles grid-zip-1)))
      (is (= [[1 4] [2 2] [1 2] [3 3]]                   (d15/get-leaves-tiles grid-zip-1)))

      ;; when robot is not close to box
      (is (= [[1 1]]                                     (d15/get-connected-tiles grid-zip-2)))
      (is (= [[1 1]]                                     (d15/get-leaves-tiles grid-zip-2)))

      (is (= [[1 3] [1 2] [2 2]]                         (d15/get-connected-tiles grid-zip-3)))
      (is (= [[1 2] [2 2]]                               (d15/get-leaves-tiles grid-zip-3)))

      ;; going down
      (is (= [[2 1] [2 2] [2 3] [2 4] [1 4] [3 3] [1 2]] (d15/get-connected-tiles grid-zip-4)))
      (is (= [[2 4] [1 4] [3 3] [1 2]]                   (d15/get-leaves-tiles grid-zip-4)))
      ;;      
      )))

(deftest vertical-move-possible-test
  (testing "Test vertical move possible"
    (let [grid   [[\# \# \# \# \#]
                  [\# \. \. \. \#]
                  [\# \[ \] \. \#]
                  [\# \. \[ \] \#]
                  [\# \[ \] \. \#]
                  [\# \. \. \. \#]
                  [\# \# \# \# \#]]]
      ;; moving up
      (is (d15/vertical-move-possible? grid d15/move-up-char [[1 2] [2 2] [3 3] [1 4]]))
      (is (d15/vertical-move-possible? grid d15/move-up-char [[1 2]]))
      (is (not (d15/vertical-move-possible? grid d15/move-up-char [[1 1]])))
      (is (not (d15/vertical-move-possible? grid d15/move-up-char [[1 2] [2 3]])))

      ;; moving down
      (is (d15/vertical-move-possible? grid d15/move-down-char [[1 2] [3 3] [1 4]]))
      (is (d15/vertical-move-possible? grid d15/move-down-char [[1 4]]))
      (is (not (d15/vertical-move-possible? grid d15/move-down-char [[1 5]])))
      (is (not (d15/vertical-move-possible? grid d15/move-down-char [[1 2] [3 3] [1 4] [1 5]])))

      ;;
      )))

(deftest vertical-update-grid-test
  (testing "update grid for one vertical move"
    ;; going down
    (is (= [[\# \# \# \# \#]
            [\# \. \. \. \#]
            [\# \@ \] \. \#]
            [\# \. \[ \] \#]
            [\# \[ \] \. \#]
            [\# \. \. \. \#]
            [\# \# \# \# \#]]

           (d15/update-on-vertical-move [[\# \# \# \# \#]
                                         [\# \@ \. \. \#]
                                         [\# \[ \] \. \#]
                                         [\# \. \[ \] \#]
                                         [\# \[ \] \. \#]
                                         [\# \. \. \. \#]
                                         [\# \# \# \# \#]]
                                        [[1 1] \@] inc)))
    ;; going up
    (is (= [[\# \# \# \# \#]
            [\# \. \. \. \#]
            [\# \[ \] \. \#]
            [\# \. \[ \] \#]
            [\# \@ \] \. \#]
            [\# \. \. \. \#]
            [\# \# \# \# \#]]

           (d15/update-on-vertical-move [[\# \# \# \# \#]
                                         [\# \. \. \. \#]
                                         [\# \[ \] \. \#]
                                         [\# \. \[ \] \#]
                                         [\# \[ \] \. \#]
                                         [\# \@ \. \. \#]
                                         [\# \# \# \# \#]]
                                        [[1 5] \@] dec)))))

(deftest move-veritcal-test
  (testing "Moving robot veritcally"
    (is (= [[\# \# \# \# \#]
            [\# \. \. \. \#]
            [\# \[ \] \. \#]
            [\# \. \[ \] \#]
            [\# \. \# \. \#]
            [\# \. \@ \. \#]
            [\# \# \# \# \#]]

           (d15/move-veritcal [[\# \# \# \# \#]
                               [\# \. \. \. \#]
                               [\# \[ \] \. \#]
                               [\# \. \[ \] \#]
                               [\# \. \# \. \#]
                               [\# \. \@ \. \#]
                               [\# \# \# \# \#]]
                              d15/move-up-char))
        "when move up is not possible"
        )
    
    (is (= [[\# \# \# \# \#]
            [\# \[ \] \. \#]
            [\# \. \[ \] \#]
            [\# \[ \] \. \#]
            [\# \. \@ \. \#]
            [\# \. \. \. \#]
            [\# \# \# \# \#]]
    
           (d15/move-veritcal [[\# \# \# \# \#]
                               [\# \. \. \. \#]
                               [\# \[ \] \. \#]
                               [\# \. \[ \] \#]
                               [\# \[ \] \. \#]
                               [\# \. \@ \. \#]
                               [\# \# \# \# \#]]
                              d15/move-up-char))
        "moving up")
    
    (is (= [[\# \# \# \# \#]
            [\# \. \. \. \#]
            [\# \. \@ \. \#]
            [\# \[ \] \. \#]
            [\# \. \[ \] \#]
            [\# \[ \] \. \#]
            [\# \# \# \# \#]]
    
           (d15/move-veritcal [[\# \# \# \# \#]
                               [\# \. \@ \. \#]
                               [\# \[ \] \. \#]
                               [\# \. \[ \] \#]
                               [\# \[ \] \. \#]
                               [\# \. \. \. \#]
                               [\# \# \# \# \#]]
                              d15/move-down-char))
        "moving down")
    
    (is (= [[\# \# \# \# \#]
            [\# \. \@ \. \#]
            [\# \[ \] \. \#]
            [\# \# \[ \] \#]
            [\# \[ \] \. \#]
            [\# \. \. \. \#]
            [\# \# \# \# \#]]
    
           (d15/move-veritcal [[\# \# \# \# \#]
                               [\# \. \@ \. \#]
                               [\# \[ \] \. \#]
                               [\# \# \[ \] \#]
                               [\# \[ \] \. \#]
                               [\# \. \. \. \#]
                               [\# \# \# \# \#]]
                              d15/move-down-char))
        "moving down not possible")
    
    (is (= [[\# \# \# \# \# \#]
            [\# \. \. \. \. \#]
            [\# \[ \] \. \. \#]
            [\# \. \[ \] \. \#]
            [\# \@ \. \. \. \#]
            [\# \[ \] \. \. \#]
            [\# \# \# \# \# \#]]

    
           (d15/move-veritcal [[\# \# \# \# \# \#]
                               [\# \. \. \. \. \#]
                               [\# \[ \] \. \. \#]
                               [\# \@ \[ \] \. \#]
                               [\# \[ \] \. \. \#]
                               [\# \. \. \. \. \#]
                               [\# \# \# \# \# \#]]
                              d15/move-down-char))
        "moving down again")
    )) 
