(ns day-8-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :refer [split-lines]]
            [day-8 :as d]))


(deftest count-edge-tree-test
  (testing "Context of the test assertions"
    (is (= 6
           (d/count-edge-trees [[1 2 3]
                                [4 5 6]])))
    (is (= 10
           (d/count-edge-trees [[1 2 3 4]
                                [5 6 7 8]
                                [9 :a :b :c]])))))


(deftest sample->grid-test
  (testing "convert sample data to grid"
    (is (=  [[1 2 3] [4 5 6] [7 8 9]]
            (d/sample->grid "123\n456\n789")))))


(deftest with-and-height-test
  (testing "get grid width and height"
    (is (= 3
           (d/grid-width [[1 2 3]
                          [4 5 6]]))))
  (testing "get grid height"
    (is (= 3
           (d/grid-width [[1 2 3]
                          [4 5 6]
                          [7 8 9]])))))


(deftest get-coll-test
  (testing "get grid col by zero based X index"
    (is (= [:a :e]
           (d/get-coll 0 [[:a :b :c :d]
                          [:e :f :g :h]])))
    (is (= [:b :e :h :k]
           (d/get-coll 1 [[:a :b :c]
                          [:d :e :f]
                          [:g :h :i]
                          [:j :k :l]])))))


(deftest get-row-test
  (testing "get grid row by zero based Y index"
    (is (= [1 2 3]
           (d/get-row 0 [[1 2 3]
                         [3 4 5]])))
    (is (= [:d :e :f]
           (d/get-row 1 [[:a :b :c]
                         [:d :e :f]])))))


(deftest split-arround-idx-test
  (testing "split a vector in before and after item at given index"
    (is (=  [[:a :b] [:d :e]]
            (d/split-around-idx [:a :b :c :d :e] 2)))
    (is (= [[] [2 3]]
           (d/split-around-idx [1 2 3] 0)))
    (is (= [[1 2] []]
           (d/split-around-idx [1 2 3] 2))))
  (testing "out of bounds idx"
    (is (= [[1 2 3] []]
           (d/split-around-idx [1 2 3] 4)))
    (is (= [[] [1 2 3]]
           (d/split-around-idx [1 2 3] -1)))))

(deftest inner-grid-xy-test
  (testing "seq of x,y pos for inner grid"
    (is (= '([1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1] [3 2] [3 3])
           (doall (d/inner-grid-xy [[1 2 3 4 5]
                                    [1 2 3 4 5]
                                    [1 2 3 4 5]
                                    [1 2 3 4 5]
                                    [1 2 3 4 5]]))))
    (is (= '([1 1] [1 2] [1 3])
           (doall (d/inner-grid-xy [[1 2 3]
                                    [1 2 3]
                                    [1 2 3]
                                    [1 2 3]
                                    [1 2 3]]))))))


(deftest get-xy-test
  (testing "get value via x,y pos in grid"
    (is (= 1
           (d/get-xy [[1 2 3]
                      [4 5 6]
                      [7 8 9]] 0 0)))
    (is (= 5
           (d/get-xy [[1 2 3]
                      [4 5 6]
                      [7 8 9]] 1 1)))
    (is (= 9
           (d/get-xy [[1 2 3]
                      [4 5 6]
                      [7 8 9]] 2 2)))
    (is (= :b
           (d/get-xy [[1 2 3 :a]
                      [4 5 6 :b]
                      [7 8 9 :c]] 3 1))))
  (testing "out of bounds x,y"
    (is (nil? (d/get-xy [[1 2]] 2 2)))
    (is (nil? (d/get-xy [[1 2]] 0 2)))
    (is (nil? (d/get-xy [[1 2]] 2 0)))))

(deftest count-visible-trees-test
  (testing "Return viewing distance"
    (is (= 4
           (count-visible-trees 4 [2 3 2 4 3])))
    (is (= 0
           (count-visible-trees 4 [])))
    (is (= 1
           (count-visible-trees 4 [5 3 2])))
    (is (= 5
           (count-visible-trees 4 [1 3 2 2 2])))
    ))