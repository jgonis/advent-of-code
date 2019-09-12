(in-package :advent-of-code-2018)
(defun problem4-1 (input-path)
  (flet ((chunk-input (sorted-lines) 
           (let  ((chunked-input (list)))
             (dolist (line sorted-lines (nreverse chunked-input))
               (if (search "Guard" line)
                   (push (make-p4-record line) chunked-input))))))
      (let* ((input-lines (aoc-utils:input->list input-path))
             (sorted-lines (sort input-lines (lambda (line1 line2) 
                                               (let* ((date-string-1 (subseq line1 1 (position #\] line1)))
                                                      (date-string-2 (subseq line2 1 (position #\] line2)))
                                                      (date-obj1 (aoc-utils:make-date-time date-string-1))
                                                      (date-obj2 (aoc-utils:make-date-time date-string-2)))
                                                 (aoc-utils:less-than date-obj1 date-obj2)))))
             (chunked-input (chunk-input sorted-lines)))
        )))

(defun make-p4-record (shift-start-line)
  (let* ((date-string (subseq shift-start-line 1 (position #\] shift-start-line)))
         (date-time (aoc-utils:make-date-time date-string))
         (shift-start-date (concatenate 'string 
                                        (write-to-string (aoc-utils:month date-time)) 
                                        "-" 
                                        (cond ((= 23 (aoc-utils:hour date-time)) 
                                               (format nil "~2,'0d" (+ 1 (aoc-utils:day date-time))))
                                              (t (format nil "~2,'0d" (aoc-utils:day date-time))))))
         (guard-id (subseq shift-start-line 
                           (+ (position #\# shift-start-line) 1)
                           (position #\Space 
                                     shift-start-line 
                                     :start (position #\# shift-start-line)))))
    (list shift-start-date guard-id (make-list 60 :initial-element #\.))))

(defun print-p4-record (p4-record)
  (format t "~A #~A ~{~A~}~%" 
          (car p4-record) 
          (car (cdr p4-record)) 
          (car (cdr (cdr p4-record)))))
