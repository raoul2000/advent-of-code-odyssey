(ns day-6-test
  (:require [clojure.test :refer [testing deftest is are]]
            [day-6 :as d6]))


(deftest loop-test
  (testing "loop predicate"
    (is (= true
           (day-6/loop? [[4 5] [0 1]] [[[6 4] [1 0]]  [[4 5] [0 1]]])))

    (is (= false
           (day-6/loop? [[4 5] [0 1]] [[[6 4] [1 0]]  [[4 6] [0 1]]])))))

(deftest add-obstruction-test
  (testing "add obstruction char"
    (is (= \. (d6/char-at
               (d6/create-grid d6/sample-input)
               [0 0])))
    (is (= \# (d6/char-at
               (d6/add-obstruction [0 0] (d6/create-grid d6/sample-input))
               [0 0])))

    (is (= (d6/create-grid d6/sample-input)
           (d6/add-obstruction [-1 0] (d6/create-grid d6/sample-input)))
        "don't modify grid when obstruction position is out of grid (1)")
    (is (= (d6/create-grid d6/sample-input)
           (d6/add-obstruction [0 10] (d6/create-grid d6/sample-input)))
        "don't modify grid when obstruction position is out of grid (2)")))

(deftest build-obstructed-path-test
  (testing "build obstructed path"

    (is (= 41
           (->> (d6/build-obstructed-path (d6/create-grid d6/sample-input) [[4 6] [0 -1]] [4 0])
                (map d6/step-position)
                set
                count))
        "no obstruction - solution 1 value is returned")

    (is (= nil
           (d6/build-obstructed-path (d6/create-grid d6/sample-input) [[4 6] [0 -1]] [3 6]))
        "testing with obstruction 1")

    (is (= nil
           (d6/build-obstructed-path (d6/create-grid d6/sample-input) [[4 6] [0 -1]] [7 9]))
        "testing with obstruction 2")

    (is (not
         (nil? (d6/build-obstructed-path (d6/create-grid d6/sample-input) [[4 6] [0 -1]] [3 5])))
        "testing with useless obstruction "))) 