(in-package :advent-of-code-2019.tests)

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

(test problem3-1
  (let ((p3path (merge-pathnames 
                 #P"local-projects/advent-of-code/2019/p3input.txt"
                 ql:*quicklisp-home*))
        (p3test1 (merge-pathnames 
                 #P"local-projects/advent-of-code/2019/p3testinput.txt"
                 ql:*quicklisp-home*))
        (p3test2 (merge-pathnames 
                 #P"local-projects/advent-of-code/2019/p3testinput2.txt"
                 ql:*quicklisp-home*))
        (p3test3 (merge-pathnames 
                 #P"local-projects/advent-of-code/2019/p3testinput3.txt"
                 ql:*quicklisp-home*)))
    (is (= (aoc-19:problem3-1 p3test1) 6))
    (is (= (aoc-19:problem3-1 p3test2) 159))
    (is (= (aoc-19:problem3-1 p3test3) 135))
    (is (= (aoc-19:problem3-1 p3path) 2193))))

(test problem3-2
  (let ((p3path (merge-pathnames 
                 #P"local-projects/advent-of-code/2019/p3input.txt"
                 ql:*quicklisp-home*)))
    ;; (is (= (aoc-19:problem3-2 p3path) 2))
    ))
