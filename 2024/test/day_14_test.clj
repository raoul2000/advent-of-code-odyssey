(ns day-14-test
  (:require [clojure.test :refer [testing deftest is are]]
            [clojure.set :as st]
            [day-14 :as d14]))


(deftest move-x-test
  (testing "Moving robot on the x axis"
    (is (= 2
           (d14/move-x {:px 0 :vx 2} 6 1)))
    (is (= 4
           (d14/move-x {:px 0 :vx 2} 6 2)))
    (is (= 0
           (d14/move-x {:px 0 :vx 2} 6 3)))
    (is (= 5
           (d14/move-x {:px 0 :vx 5} 6 1)))
    (is (= 4
           (d14/move-x {:px 0 :vx 5} 6 2)))
    (is (= 3
           (d14/move-x {:px 0 :vx 5} 6 3)))
    
    (is (= 3
           (d14/move-x {:px 0 :vx -2} 6 3)))
    )) 