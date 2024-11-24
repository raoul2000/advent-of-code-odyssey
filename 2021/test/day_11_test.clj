(ns day-11-test
  (:require [clojure.test :refer [deftest testing is]]
            day-11))

(deftest navigation-test
  (testing "same line"
    (is (true? (day-11/same-line 0 1))))
  )