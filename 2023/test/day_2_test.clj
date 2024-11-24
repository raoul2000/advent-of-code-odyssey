(ns day-2-test
  (:require [clojure.test :refer [deftest testing is are]]
            [day-2 :as d]))



(deftest game-id-test
  (testing "get game id from handful"
    (are [result handful] (= result (d/game-id handful))
      0       ""
      1       "Game 1: some other value"
      99      "Game 99: 9 red, 3 green, 10 blue; 10 red, 10 blue, 4 green; 2 green, 15 blue, 3 red; 12 blue, 4 red"
      100     "Game 100: 15 blue, 6 red; 1 green, 2 red; 12 blue, 8 green, 1 red; 1 red, 7 blue"))) 




