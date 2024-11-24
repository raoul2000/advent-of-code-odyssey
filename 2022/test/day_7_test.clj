(ns day-7-test
  (:require [clojure.test :refer [deftest testing is]]
            [day-7 :as d]))

(deftest add-file-to-cur-dir-test
  (testing "Context of the test assertions"
    (is (= {:cur {"dir" '(["22" "file.txt"])}}
           (d/add-file-to-cur-dir {:cur "dir"} ["22" "file.txt"])))))


(deftest cd-path-test
  (testing "update path with cd command"
    (is (= "/a/b/c"
           (d/cd-path "/a/b/c/d" "..")))
    (is (= "/"
           (d/cd-path "/a" "..")))
    (is (= "/"
           (d/cd-path "/whatever" "/")))
    (is (= "/"
           (d/cd-path "/" "..")))
    (is (= "/a/b/c"
           (d/cd-path "/a/b" "c")))
    (is (= "/a"
           (d/cd-path "/" "a")))))

