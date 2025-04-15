(ns day-14-test
  (:require [clojure.test :refer [testing deftest is are]]
            [clojure.set :as st]
            [day-14 :as d14]))


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

(deftest move-robot-test
  (testing "Moving robot"
    (is (= {:px 1, :vx 1, :py 1, :vy 1}
           (d14/move-robot {:px 0 :vx 1
                            :py 0 :vy 1} 2 2 1)))

    (is (= {:px 1, :vx 2, :py 3, :vy -3}
           (d14/move-robot {:px 2, :vx 2, :py 4, :vy -3} 11 7 5)))))


(deftest grid-spec-test
  (testing "creating grid spec"
    (is (= {:x-fronter 5, :y-fronter 3, :col-count 11, :row-count 7}
           (d14/grid-spec 11 7)))

    (is (= {:x-fronter 50, :y-fronter 51, :col-count 101, :row-count 103}
           (d14/grid-spec 101 103)))))

(deftest quadrant-predicates-test
  (testing "quadrant predicates"
    (is (= true
           (d14/q1? (d14/grid-spec 11 7) {:px 0 :py 0})))
    (is (= false
           (d14/q2? (d14/grid-spec 11 7) {:px 0 :py 0})))
    (is (= false
           (d14/q3? (d14/grid-spec 11 7) {:px 0 :py 0})))
    (is (= false
           (d14/q4? (d14/grid-spec 11 7) {:px 0 :py 0})))

    (are [result arg-map] (= result ((:q arg-map) (d14/grid-spec 11 7)   arg-map))
      ;; q1
      true     {:q d14/q1? :px 0 :py 0}
      false    {:q d14/q2? :px 0 :py 0}
      false    {:q d14/q3? :px 0 :py 0}
      false    {:q d14/q4? :px 0 :py 0}
      ;; on x-fronter
      false    {:q d14/q1? :px 5 :py 0}
      false    {:q d14/q2? :px 5 :py 0}
      false    {:q d14/q3? :px 5 :py 0}
      false    {:q d14/q4? :px 5 :py 0}

      ;; q2
      false    {:q d14/q1? :px 7 :py 0}
      true     {:q d14/q2? :px 7 :py 0}
      false    {:q d14/q3? :px 7 :py 0}
      false    {:q d14/q4? :px 7 :py 0}

      ;; q3
      false    {:q d14/q1? :px 2 :py 4}
      false    {:q d14/q2? :px 2 :py 4}
      true     {:q d14/q3? :px 2 :py 4}
      false    {:q d14/q4? :px 2 :py 4}

      ;; on y-fronter
      false    {:q d14/q1? :px 2 :py 3}
      false    {:q d14/q2? :px 2 :py 3}
      false    {:q d14/q3? :px 2 :py 3}
      false    {:q d14/q4? :px 2 :py 3}

      ;; q4
      false    {:q d14/q1? :px 6  :py 4}
      false    {:q d14/q2? :px 6  :py 4}
      false    {:q d14/q3? :px 6  :py 4}
      true     {:q d14/q4? :px 6  :py 4}
      true     {:q d14/q4? :px 10 :py 6})
    ;;
    ))

((deftest count-consecutive-by-axis-test
   (testing "Count consecutive robots on given axis"
     (is (=  3
             (d14/count-consecutive-by-axis :px [{:px 2} {:px 3} {:px 4}])))
     (is (=  3
             (d14/count-consecutive-by-axis :px [{:px 2} {:px 4} {:px 3}])))
     (is (=  0
             (d14/count-consecutive-by-axis :px [{:px 2} {:px 4} {:px 30} {:px 11} {:px 13}])))
     (is (=  2
             (d14/count-consecutive-by-axis :px [{:px 3} {:px 40} {:px 30} {:px 4} {:px 31}])))
     (is (=  2
             (d14/count-consecutive-by-axis :px [{:px 3} {:px 40} {:px 30} {:px 4} {:px 4} {:px 4} {:px 4}]))
         "when robots overlap"))))

