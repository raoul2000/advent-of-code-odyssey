(ns day-12-test
  (:require [clojure.test :refer [testing deftest is are]]
            [clojure.set :as st]
            [day-12 :as d12]))

#_(st/difference #{[0 0] [2 0] [1 0] [3 0]})

(def region-1 #{[0 0] [1 0] [3 0] [2 0]})

(deftest sides-finder
  (test "simple sides"
        (is (= #{[0 -1] [1 -1] [2 -1] [3 -1]})
            (d12/sides region-1))))