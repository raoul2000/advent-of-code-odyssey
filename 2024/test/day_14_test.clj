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

    ;; negative velocity
    (is (= 4
           (d14/move-x {:px 0 :vx -2} 6 1)))
    (is (= 2
           (d14/move-x {:px 0 :vx -2} 6 2)))
    (is (= 0
           (d14/move-x {:px 0 :vx -2} 6 3)))
    (is (= 4
           (d14/move-x {:px 0 :vx -2} 6 4)))

    (is (= 4
           (d14/move-x {:px 2 :vx -10} 6 1)))))

(deftest move-on-axis-test
  (testing "move robot on single axis"
    (are [result arg-map] (= result (d14/move-on-axis (:coord arg-map) (:v arg-map) (:axis-size arg-map) (:sec-count arg-map)))
      1      {:coord 0 :v 1 :axis-size 2 :sec-count 1}
      0      {:coord 0 :v 1 :axis-size 2 :sec-count 2}
      1      {:coord 0 :v 1 :axis-size 2 :sec-count 3}

      0      {:coord 5 :v 1 :axis-size 6 :sec-count 1}
      1      {:coord 5 :v 2 :axis-size 6 :sec-count 1}
      3      {:coord 5 :v 2 :axis-size 6 :sec-count 2}

      1      {:coord 0 :v -1 :axis-size 2 :sec-count 1}
      0      {:coord 0 :v -1 :axis-size 2 :sec-count 2}

      3      {:coord 1 :v -4 :axis-size 6 :sec-count 1}
      5      {:coord 1 :v -4 :axis-size 6 :sec-count 2}))) 