(in-package :aoc-utils)
(defun make-growable-vector (initial-size) 
    (make-array initial-size :fill-pointer 0 :adjustable t))
