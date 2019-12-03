(in-package :advent-of-code.tests)

(test problem1-1
  (let ((p1path (merge-pathnames 
                 #P"local-projects/advent-of-code/2019/p1input.txt"
                 ql:*quicklisp-home*)))
    (is (= (aoc-19::problem1-helper 12 :include-fuel nil) 2))
    (is (= (aoc-19::problem1-helper 14 :include-fuel nil) 2))
    (is (= (aoc-19::problem1-helper 1969 :include-fuel nil) 654))
    (is (= (aoc-19::problem1-helper 100756 :include-fuel nil) 33583))
    (is (= (aoc-19:problem1-1 p1path) 3297896))))

(test problem1-2
  (let ((p1path (merge-pathnames 
                 #P"local-projects/advent-of-code/2019/p1input.txt"
                 ql:*quicklisp-home*)))
    (is (= (aoc-19::problem1-helper 14 :include-fuel t) 2))
    (is (= (aoc-19::problem1-helper 1969 :include-fuel t) 966))
    (is (= (aoc-19::problem1-helper 100756 :include-fuel t) 50346))
    (is (= (aoc-19:problem1-2 p1path) 4943969))))

(test problem2-1
  (let ((p2path (merge-pathnames 
                 #P"local-projects/advent-of-code/2019/p2input.txt"
                 ql:*quicklisp-home*)))
    (is (= (aoc-19:problem2-1 p2path) 19690720))))

(test problem2-2
  (let ((p2path (merge-pathnames 
                 #P"local-projects/advent-of-code/2019/p2input.txt"
                 ql:*quicklisp-home*)))
    (is (= (aoc-19:problem2-2 p2path) 4259))))
