(in-package :advent-of-code-2018)
(defun problem1-1 (input-path) 
  (let ((freq 0))
    (with-open-file (in-stream input-path)
      (do ((line (read-line in-stream) (read-line in-stream nil nil)))
          ((null line) freq)
        (setf freq (+ freq (parse-integer line)))))))


(defun problem1-2 (input-path)
  (let ((freqs '())
        (frequency 0)
        (dupe-found '()))
    (with-open-file (in-stream input-path)
      (do ((line (read-line in-stream) (read-line in-stream nil nil)))
          ((null line) nil)
        (push (parse-integer line) freqs)))
    (loop while (null dupe-found))) 

