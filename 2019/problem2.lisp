(in-package :advent-of-code-2019)
(defun problem2-1 (input-path)
  (let* ((op-codes (p2-parsing-helper input-path)))
    (setf (elt op-codes 1) 42)
    (setf (elt op-codes 2) 59)
    (program-runner op-codes)
    (elt op-codes 0)))

(defun problem2-2 (input-path)
  (let* ((op-codes (p2-parsing-helper input-path))
         (noun-values (alexandria:iota 100))
         (verb-values (alexandria:iota 100)))
    (dolist (noun-value noun-values)
      (dolist (verb-value verb-values)
        (let ((op-codes-copy (copy-seq op-codes)))
          (setf (elt op-codes-copy 1) noun-value)
          (setf (elt op-codes-copy 2) verb-value)
          (program-runner op-codes-copy)
          (if (= (elt op-codes-copy 0) 19690720)
              (return-from problem2-2 
                (+ (* 100 noun-value) verb-value))))))))

(defun p2-parsing-helper (input-path)
  (let* ((input-line (aoc-utils:input->vec input-path))
         (input-strings (cl-utilities:split-sequence 
                         #\, 
                         (elt input-line 0)))
         (input-numbers (map 'vector
                             #'parse-integer
                             input-strings)))
    input-numbers))

(defun program-runner (op-codes)
  (let ((add-op (make-instance 'add-op))
        (multiply-op (make-instance 'multiply-op)))
      (labels ((helper (op-codes current-idx)
                 (let ((op-code (elt op-codes current-idx)))
                   (cond ((= op-code 99) op-codes)
                         ((= op-code 1) (handle-op add-op
                                                   op-codes
                                                   current-idx)
                          (helper op-codes (increment-pc add-op
                                                         current-idx)))
                         ((= op-code 2) (handle-op multiply-op
                                                   op-codes
                                                   current-idx)
                          (helper op-codes (increment-pc multiply-op
                                                         current-idx)))
                         (t (error "unknown opcode ~A~%" 
                                   op-code))))))
        (helper op-codes 0))))
