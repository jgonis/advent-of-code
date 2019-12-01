(in-package :advent-of-code-2019)
(defun problem1-1 (input-path)
  (let* ((input-lines (aoc-utils:input->list input-path))
         (input-numbers (map 'list 
                             (lambda (x) (parse-integer x)) 
                             input-lines))
         (fuel-reqs (map 'list
                         (lambda (x) (problem1-1-helper x))
                         input-numbers)))
    (reduce #'+ fuel-reqs)))

(defun problem1-1-helper (mass)
  (- (floor (/ mass 3)) 2))

(defun problem1-2 (input-path)
  (let* ((input-lines (aoc-utils:input->list input-path))
        (input-numbers (map 'list
                            (lambda (x) (parse-integer x))
                            input-lines))
        (fuel-reqs 
         (map 'list
              (lambda (x) 
                (problem1-2-helper x))
              input-numbers)))
    (reduce #'+ fuel-reqs)))

(defun problem1-2-helper (mass)
  (let ((initial-mass (problem1-1-helper mass)))
    (labels ((helper (mass)
               (let ((additional-mass (problem1-1-helper mass)))
                 (cond ((<= additional-mass 0) 0)
                       (t (+ additional-mass 
                             (helper additional-mass)))))))
      (+ initial-mass (helper initial-mass)))))

