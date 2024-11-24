(ns day-10-part-2-again
  (:require [day-10 :refer [find-possible-next-steps
                            create-grid
                            in-grid
                            find-S-pos
                            walk-the-pipes
                            sample-input-1
                            sample-input-2]]))

;; OK I have been struggling to solve this part 2 and after my two previous failure
;; I was thinking about a possible alternate solution based on the first attemps.
;; If I expand (some sort of zomm in) the grid so to not have anymore pipe tiles sticked to each other
;; then the bst floofill algo could work.

;; turning this grid :
;; .S------7.
;; .|F----7|.
;; .||....||.
;; .|L-7F-J|.
;; .|..||..|.
;; .L--JL--J.

;; ..into this one (expanded)

;; *******************
;; .*S-------------7*.
;; .*|*F---------7*|*.
;; .*|*|*.*.*.*.*|*|*.
;; .*|*L---7*F---J*|*.
;; .*|*.*.*|*|*.*.*|*.
;; .*L-----J*L-----J*.
;; *******************

;; Then apply flood fill and compute
;; wait. Should I expand also the rows ? 

;; Well I don't have that much time to continue for now .... I will stop here.

