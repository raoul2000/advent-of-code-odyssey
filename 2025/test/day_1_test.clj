(ns day-1-test
  (:require [clojure.test :refer [testing deftest is are]]
            [day-1 :as d1]))


(deftest apply-rotation-test
  (testing "Apply rotation to current dial"
    (are [result arg-map] (= result (d1/apply-rotation [(:dir arg-map) (:dist arg-map)]
                                                       (:dial arg-map)))
      0   {:dir "L"  :dist 0 :dial 0}
      0   {:dir "R"  :dist 0 :dial 0}
      99  {:dir "L"  :dist 1 :dial 0}
      1   {:dir "R"  :dist 1 :dial 0}
      50  {:dir "L"  :dist 50 :dial 0}
      50  {:dir "R"  :dist 50 :dial 0}
      0   {:dir "L"  :dist 100 :dial 0}
      0   {:dir "R"  :dist 100 :dial 0}
      82  {:dir "L"  :dist 68 :dial 50}
      52  {:dir "L"  :dist 30 :dial 82}
      0   {:dir "R"  :dist 48 :dial 52}
      95  {:dir "L"  :dist 5 :dial 0}
      55  {:dir "R"  :dist 60 :dial 95}
      0   {:dir "L"  :dist 55 :dial 55}

      ;;
      )))

(deftest cross-zero-count-test
  (testing "Count how many zero crossed"
    (are [result arg-map] (= result (d1/cross-zero-count [(:dir arg-map) (:dist arg-map)]
                                                         (:dial arg-map)))
      0   {:dir "L"  :dist 0 :dial 0}
      0   {:dir "L"  :dist 1 :dial 1} ;; end on zero but do not cross
      1   {:dir "L"  :dist 2 :dial 1}
      0   {:dir "R"  :dist 1 :dial 99} ;; end on zero but do not cross
      1   {:dir "R"  :dist 2 :dial 99}
      1   {:dir "R"  :dist 51 :dial 50}
      1   {:dir "L"  :dist 51 :dial 50}
      ;; more than one cross
      1   {:dir "L"  :dist 110 :dial 1}



      ;;
      ))) 