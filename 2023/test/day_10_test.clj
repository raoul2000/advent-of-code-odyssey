(ns day-10-test
  (:require [clojure.test :refer [deftest testing are is]]
            [day-10 :as d]))



(def sample-input-1
  "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
")
(def grid-1 (d/create-grid sample-input-1))
;; .....
;; .F-7.
;; .|.|.
;; .L-J.
;; .....


(def sample-input-2
  "-L|F7
7S-7|
L|7||
-L-J|
L|-JF
")
(def grid-2 (d/create-grid sample-input-2))
;; ..F7.
;; .FJ|.
;; SJ.L7
;; |F--J
;; LJ...
;; 


(deftest helper-test
  (testing "Complement direction"
    (is (= :north (d/complement-direction :south)))
    (is (= :south (d/complement-direction :north)))
    (is (= :west  (d/complement-direction :east)))
    (is (= :east  (d/complement-direction :west)))
    (is (thrown? Exception (d/complement-direction :north-west))))

  (testing "find matching pipe"
    (is (nil?          (d/find-matching-pipes \| :east)))
    (is (= #{\F \7 \|} (d/find-matching-pipes \| :north)))
    (is (= #{\J \- \7} (d/find-matching-pipes \F :east)))
    (is (= #{\J \L \|} (d/find-matching-pipes \F :south)))
    (is (nil?          (d/find-matching-pipes \F :north)))
    (is (nil?          (d/find-matching-pipes \- :north))))

  (testing "adjacent coords"
    (is (= '([:east [1 0] \-] [:south [0 1] \.])
           (d/adjacent-coords [0 0] grid-1)))
    (is (= '([:east [2 1] \J] [:west [0 1] \.] [:south [1 2] \J] [:north [1 0] \-])
           (d/adjacent-coords [1 1] grid-1)))
    (is (= '([:west [3 4] \L] [:north [4 3] \J])
           (d/adjacent-coords [4 4] grid-1)))
    (is (= '([:east [2 2] \L] [:west [0 2] \S] [:south [1 3] \F] [:north [1 1] \F])
           (d/adjacent-coords [1 2] grid-1)))))

(deftest walk-the-pipes-test
  (testing "finding position of start"
    (is (= [1 1]
           (d/find-S-pos (d/create-grid "123\n4S5\n678"))))
    (is (= [0 0]
           (d/find-S-pos (d/create-grid "S23\n4S5\n678"))))
    (is (= [2 2]
           (d/find-S-pos (d/create-grid "123\n4X5\n67S"))))
    (is (nil?
         (d/find-S-pos (d/create-grid "123\n4X5\n67Y")))))

  (testing "finding all possible next steps walking the pipes"
    (is (empty?
          (d/find-possible-next-steps grid-1 [0 0] nil)))
     (is (= '([:east [2 1] \J] [:south [1 2] \J])
          (d/find-possible-next-steps grid-1 [1 1] nil)))))
 