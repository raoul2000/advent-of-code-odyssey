(ns day-11-test
  (:require [clojure.test :refer [testing deftest is are]]
            [day-11 :as d11]))

(deftest blink-simple
  (testing "blink a single stone "
    (is (= {"2024" 1}
           (d11/blink-stone {"1" 1}))
        "simple multiply by 2024")

    (is (= {"1" 1}
           (d11/blink-stone {"0" 1}))
        "simple set to 1")

    (is (= {"20" 1
            "24" 1}
           (d11/blink-stone {"2024" 1}))
        "simple split")

    (is (= {"10" 1
            "0"  1}
           (d11/blink-stone {"1000" 1}))
        "00 is turned into 0")
    
    (is (= {"10" 4
            "0"  4}
           (d11/blink-stone {"1000" 4}))
        "00 is turned into 0")))

(deftest blink-multi
  (testing "blink multiple stones"
    (is (= {"2024" 1
            "1"    1}
           (d11/blink-stone {"1" 1
                             "0" 1})))

    (is (= {"20"    1
            "24"    1
            "4048"  1}
           (d11/blink-stone {"2024" 1
                             "2" 1})))

    (is (= {"20"    1
            "24"    1
            "40"    1
            "48"    1}
           (d11/blink-stone {"2024" 1
                             "4048" 1})))))

(deftest blink-multi-occurence
  (testing "blink multiple with multi occurences sum"
    (is (= {"0" 1
            "1" 2
            "2" 1}
           (d11/blink-stone {"10" 1
                             "21" 1})))

    (is (= {"0" 2
            "1" 3
            "2" 1}
           (d11/blink-stone {"10" 2
                             "21" 1})))

    (is (= {"10" 1
            "1"  2
            "0"  3}
           (d11/blink-stone {"10" 2
                             "1000" 1})))
    
    (is (= {"10" 4
            "1"  2
            "0"  3
            "20" 3}
           (d11/blink-stone {"10"   2
                             "1000" 1
                             "2010" 3})))
    
(is (= {"4048" 5
        "7"  3
        "2"  3}
       (d11/blink-stone {"2"   5
                         "72"  3})))
    
    (is (= {"10" 13
            "0"  3}
           (d11/blink-stone {"1010"  5
                             "1000"  3})))

    ))