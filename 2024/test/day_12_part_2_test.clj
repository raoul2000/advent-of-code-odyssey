(ns day-12-part-2-test
  (:require [clojure.test :refer [testing deftest is are]]
            [clojure.set :as st]
            [day-12-part-2 :as d12p2]))

#_(st/difference #{[0 0] [2 0] [1 0] [3 0]})

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
  (test "simple sides"
        (is (= #{[0 -1] [1 -1] [2 -1] [3 -1]})
            (d12p2/find-fences garden-1 region-A-g1))))