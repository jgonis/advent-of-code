(in-package :advent-of-code-2018)
(defun problem1-1 (input-path) 
  (let ((freq 0))
    (with-open-file (in-stream input-path)
      (do ((line (read-line in-stream) (read-line in-stream nil nil)))
          ((null line) freq)
        (setf freq (+ freq (parse-integer line)))))))


(defun problem1-2 (input-path)
  (let ((freq-offsets '()))
    (with-open-file (in-stream input-path)
      (do ((line (read-line in-stream) (read-line in-stream nil nil)))
          ((null line) nil)
        (push (parse-integer line) freq-offsets)))
    (setf freq-offsets (reverse freq-offsets))
    (setf (cdr (last freq-offsets)) freq-offsets)
    (problem1-2helper freq-offsets)))

(defun problem1-2helper (offset-list)
  (let ((calculated-freqs (make-hash-table))
        (current-freq 0))
    (dolist (offset offset-list)
      (setf current-freq (+ offset current-freq))
      (multiple-value-bind (value present) (gethash current-freq 
                                                    calculated-freqs)
        (cond (present (return current-freq))
              (t 
               (setf (gethash current-freq 
                              calculated-freqs) 
                     current-freq)))))))

(defun problem2-1 (input-path)
  (let ((two-count 0)
        (three-count 0))
    (with-open-file (in-stream input-path)
      (do ((line (read-line in-stream) (read-line instream nil nil))) 
          ((null line))
        (let ((letter-count (make-hash-table)))
          ())))))
(defun problem2-2 (input-path))
    
