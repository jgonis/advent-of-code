(in-package :advent-of-code.tests)
(defun run-tests ()
  (1am:run))

(test problem1
  (format t "~A" ql:*quicklisp-home*)
  (let ((p1path (merge-pathnames ql:*quicklisp-home* #P"local-projects/advent-of-code/2018/p1input.txt")))
    (is (= 538 (aoc-18:problem1-1 p1path)))
    (is (= 77271 (aoc-18:problem1-2 p1path)))))

(test problem2
  (format t "~A" ql:*quicklisp-home*)
  (let ((p2path (merge-pathnames ql:*quicklisp-home* #P"local-projects/advent-of-code/2018/p2input.txt")))
    (is (= 6225 (aoc-18:problem2-1 p2path)))
    (is (string= "revtaubfniyhsgxdoajwkqilp" (aoc-18:problem2-2 p2path)))))

(test problem3
  (format t "~A" ql:*quicklisp-home*)
  (let ((p3path (merge-pathnames ql:*quicklisp-home* #P"local-projects/advent-of-code/2018/p3input.txt")))
    (is (= 101469 (aoc-18:problem3-1 p3path)))
    (is (string= "1067" (aoc-18:problem3-2 p3path)))))

(test problem4
  (is (= 1 1)))
 
