(in-package :advent-of-code-2019)
(defun problem2-1 (input-path)
  (let* ((op-codes (parsing-helper input-path)))
    (setf (elt op-codes 1) 42)
    (setf (elt op-codes 2) 59)
    (program-runner op-codes)
    (elt op-codes 0)))

(defun problem2-2 (input-path)
  (let* ((op-codes (parsing-helper input-path))
         (noun-values (alexandria:iota 100))
         (verb-values (alexandria:iota 100)))
    (dolist (noun-value noun-values)
      (dolist (verb-value verb-values)
        (let ((op-codes-copy (copy-list op-codes)))
          (setf (elt op-codes-copy 1) noun-value)
          (setf (elt op-codes-copy 2) verb-value)
          (program-runner op-codes-copy)
          (if (= (elt op-codes-copy 0) 19690720)
              (return-from problem2-2 
                (+ (* 100 noun-value) verb-value))))))))

(defun parsing-helper (input-path)
  (let* ((input-line (aoc-utils:input->list input-path))
         (input-strings (cl-utilities:split-sequence #\, 
                                                     (car input-line)))
         (input-numbers (map 'list
                             #'parse-integer
                             input-strings)))
    input-numbers))

(defun program-runner (op-codes)
  (labels ((helper (op-codes current-idx)
             (let ((op-code (elt op-codes current-idx)))
               (cond ((= op-code 99) op-codes)
                     ((= op-code 1) (do-op op-codes 
                                      current-idx
                                      #'+)
                      (helper op-codes (+ current-idx 4)))
                     ((= op-code 2) (do-op op-codes
                                      current-idx
                                      #'*)
                      (helper op-codes (+ current-idx 4)))
                     (t (error "unknown opcode ~A~%" 
                               op-code)))))
           (do-op (op-codes idx operation)
             (let* ((operand-1 (elt op-codes (elt op-codes (+ idx 1))))
                    (operand-2 (elt op-codes (elt op-codes (+ idx 2))))
                    (result-location (elt op-codes (+ idx 3)))
                    (op-result (funcall operation operand-1 operand-2)))
               (setf (elt op-codes result-location)
                     op-result))))
    (helper op-codes 0)))
