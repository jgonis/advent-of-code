(in-package :advent-of-code-2019)
(defun problem1-1 (input-path)
  (let* ((input-lines (aoc-utils:input->list input-path))
         (input-numbers (map 'list 
                             (lambda (x) (parse-integer x)) 
                             input-lines))
         (fuel-reqs (map 'list
                         (lambda (x) (problem1-helper x))
                         input-numbers)))
    (reduce #'+ fuel-reqs)))

(defun problem1-2 (input-path)
  (let* ((input-lines (aoc-utils:input->list input-path))
        (input-numbers (map 'list
                            (lambda (x) (parse-integer x))
                            input-lines))
        (fuel-reqs 
         (map 'list
              (lambda (x) 
                (problem1-helper x :include-fuel t))
              input-numbers)))
    (reduce #'+ fuel-reqs)))

(defun problem1-helper (mass &key (include-fuel nil arg-supplied-p))
  (let ((initial-mass (- (floor (/ mass 3)) 2)))
    (labels ((helper (mass current-mass)
               (let ((additional-mass (- (floor (/ mass 3)) 2)))
                 (cond ((<= additional-mass 0) current-mass)
                       (t (helper additional-mass 
                                  (+ current-mass
                                     additional-mass)))))))
      (cond (include-fuel (helper initial-mass initial-mass))
            (t initial-mass)))))

