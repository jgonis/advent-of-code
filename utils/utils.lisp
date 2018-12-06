(in-package :aoc-utils)
(defun make-growable-vector (initial-size) 
    (make-array initial-size :fill-pointer 0 :adjustable t))

(defun string->list (str)
  (map 'list (lambda (x) x) str))
