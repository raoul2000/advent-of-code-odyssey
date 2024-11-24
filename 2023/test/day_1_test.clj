(ns day-1-test
  (:require [clojure.test :refer [deftest testing is are]]
            [day-1 :as d]))

(deftest calibration-linto-to-value-test
  (testing "compute calibration value"
    (are [value line] (= value (d/calibration-value-2 line))
      12      "12"
      12      "abc12def"
      12      "abc12"
      12      "12def"
      12      "abc1x2def"
      12      "1x2def"
      12      "abc1x2"
      46      "46brthree51bhvhtcnpcffoursix"
      53      "five7bhsfdktxq33qtrmvqxfgone3"
      58      "five6npfmggbdkljqsixjnxgk1cqdmcneight"
      11      "one"
      11      "oneone"
      22      "two"
      33      "three"
      44      "four"
      55      "five"
      66      "six"
      77      "seven"
      88      "eight"
      99      "nine"

      29      "two1nine"
      83      "eightwothree"
      13      "abcone2threexyz"
      24      "xtwone3four"
      42      "4nineeightseven2"
      14      "zoneight234" 
      76      "7pqrstsixteen"
      
      79      "tvpmseven3fthlkndskdgfhjrvcxcninetdlkgrnxpm"
      16      "jvoneight7eightdfvljxthreethreefoureightkvb6"
      68      "sixz12cvqlmqqzvnnhfiveoneightm"))) 