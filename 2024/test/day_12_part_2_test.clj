(ns day-12-part-2-test
  (:require [clojure.test :refer [testing deftest is are]]
            [clojure.set :as st]
            [day-12-part-2 :as d12p2]))

(def garden-1 (d12p2/create-garden "AAAA
BBCD
BBCC
EEEC
"))

(comment
  (d12p2/find-all-regions garden-1)
  ;;
  )
(def region-A-g1 #{[0 0] [1 0] [3 0] [2 0]})

(deftest find-fences
  (testing "simple sides"
    (is (= '([-0.25 0] [0.25 0] [0 -0.25] [0 0.25])
           (d12p2/find-fences #{[0 0]}))
        "on a 1 plant region")

    (is (= '([-0.25 0] [0.25 0] [0 -0.25]
                       [-0.25 1] [0.25 1] [0 1.25])
           (d12p2/find-fences #{[0 0] [0 1]}))
        "on a 2 plants vertical  region")

    (is (= '([-0.25 0] [0 -0.25] [0 0.25] [1.25 0] [1 -0.25] [1 0.25])
           (d12p2/find-fences #{[0 0] [1 0]}))
        "on a 2 plants horizontal  region")))

(deftest find-vertical-sides
  (testing "finding vertical sides"
    (is (empty?
         (d12p2/find-vertical-sides #{[0 0]}))
        "single fence has no vertical side")

    (is (= '([[-0.25 0] [-0.25 1]] [[0.25 0] [0.25 1]])
           (d12p2/find-vertical-sides #{[0 0] [0 1]}))
        "vertical side")))

(deftest find-horizontal-sides
  (testing "finding horizontal sides"
    (is (empty? (d12p2/find-horizontal-sides #{[0 0]}))
        "single fence has no horizontal sides")

    (is (=  '([[0 -0.25] [1 -0.25]] [[0 0.25] [1 0.25]])
            (d12p2/find-horizontal-sides #{[0 0] [1 0]}))
        "horizontal sides")))