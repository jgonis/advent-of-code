(in-package :advent-of-code.tests)

(test problem1-1
  (let ((p1path (merge-pathnames 
                 #P"local-projects/advent-of-code/2019/p1input.txt"
                 ql:*quicklisp-home*)))
    (is (= (aoc-19:problem1-1 p1path) 1))
    (is (= (aoc-19:problem1-2 p1path) 2))))
