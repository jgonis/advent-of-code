(in-package :advent-of-code.test)
(defun run-tests ()
  (1am:run))

(test problem1-1
  (let ((p1path #P"/home/jeff.gonis/quicklisp/local-projects/advent-of-code/2018/p1input.txt"))
    (format t "~%~A~%" (aoc-18:problem1-1 p1path))))

