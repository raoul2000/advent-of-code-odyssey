(ns day-5-test
  (:require [clojure.test :refer [deftest testing are is]]
            [day-5 :as d]))


(deftest range-comparaison-test
  (testing " included range ? "
    (are [result arg1 arg2]  (= result (d/included? arg1 arg2))
      true  [1 1] [1 1]
      true  [1 2] [1 2]
      true  [1 2] [1 3]
      true  [2 2] [1 3]
      true   [18 24] [18 25]
      false [2 4] [1 3]
      false [1 5] [2 7]
      false [3 8] [2 7]))

  (testing " left overlap range ? "
    (are [result arg1 arg2]  (= result (d/left-overlap? arg1 arg2))
      false  [1 1] [5 9]
      true   [1 5] [5 9]
      true   [1 9] [5 9]
      true   [1 10] [5 9]
      false   [18 24] [18 25]
      false   [5 5] [5 9] ;; included 
      ))

  (testing " right overlap range ? "
    (are [result arg1 arg2]  (= result (d/right-overlap? arg1 arg2))
      false  [1 1]   [5 9]
      false  [10 10] [5 9]
      true   [9 10] [5 9]
      true   [1 10] [5 9]
      false  [1 9] [5 9]  ;; included !
      )))

(deftest range-test
  (testing " Creating ranges "
    (is (= [79 92]
           (d/create-range [79 14])))
    (is (= [79 79]
           (d/create-range [79 1])))))


(deftest intersaction-test
  (testing " included "
    (is (= [[1 10] []]
           (d/intersection [1 10] [1 10])) "
--- [...] --
--- [...] -- ")

    (is (= [[1 9] []]
           (d/intersection [1 9] [1 10])) "
--- [...] --
--- [..] -- ")
    (is (= [[2 10] []]
           (d/intersection [2 10] [1 10])) "
--- [...] ---
---- [..] --- ")
    (is (= [[2 9] []]
           (d/intersection [2 9] [1 10])) "
--- [...] ---
---- [.] ---
")
    (is (= [[1 1] []]
           (d/intersection [1 1] [1 10]))))

  (testing " left and right overlap "
    (is (= [[5 15] [[1 4] [16 20]]]
           (d/intersection [1 20] [5 15]))
        "
---- [...] ----
--- [.....] ---
"))

  (testing " left overlap "
    (is (= [[5 10] [[1 4]]]
           (d/intersection [1 10] [5 15]))
        "
---- [...] ----
--- [..] ------
")
    (is (= [[10 10] [[1 9]]]
           (d/intersection [1 10] [10 15]))
        "
---- [...] ----
-- [.] ------
")
    (is (= [[10 15] [[1 9]]]
           (d/intersection [1 15] [10 15]))
        "
---- [...] ----
-- [.....] ------
"))

  (testing " right overlap "
    (is (= [[10 15] [[16 20]]]
           (d/intersection [10 20] [5 15]))
        "
---- [...] ----
------- [...] ------
")
    (is (= [[25 25] [[26 30]]]
           (d/intersection [25 30] [20 25]))
        "
---- [...] ----
--------- [...] ------
")
    (is (= [[10 15] [[16 20]]]
           (d/intersection [10 20] [10 15]))
        "
---- [...] ----
----- [......] ------
"))

  (testing " no overlap "
    (is (= [[] [[1 5]]]
           (d/intersection [1 5] [10 15]))
        "
----------- [..] ------
---- [..] ---- ")
    (is (= [[] [[20 25]]]
           (d/intersection [20 25] [10 15]))
        "
---- [..] ----
----------- [..] ------ ")))



(deftest apply-shift-rule-test
  (testing " applying rule on included "
    (is (= {:remain [], :mapped [81 94]}
           (d/apply-shift-rule-1  [79 92] [[50 97] 2])))
    (is (= {:remain [], :mapped [18 87]}
           (d/apply-shift-rule-1  [25 94]  [[25 94] -7]))))

  (testing " applying rule left overlap "
    (is (= {:remain [[5 14]], :mapped [17 22]}
           (d/apply-shift-rule-1  [5 20] [[15 25] 2])))

    (is (= {:remain [[5 19]], :mapped [22 22]}
           (d/apply-shift-rule-1  [5 20] [[20 25] 2])))))
