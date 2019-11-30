(in-package :aoc-utils)

(defun make-date-time (date-time-string)
  (multiple-value-bind (y mon d h min) (parse-time-string date-time-string)
    (local-time:encode-timestamp 0 0 min h d mon y)))
    
(defun parse-time-string (line)
  (let ((date-string (subseq line 
                             0 
                             (position #\Space line)))
        (time-string (subseq line 
                             (+ (position #\Space line) 1) 
                             (length line))))
    
    (let ((year (parse-integer (subseq date-string 
                                       0 
                                       (position #\- date-string))))
          (month (parse-integer 
                  (subseq date-string 
                          (+ (position #\- date-string) 1) 
                          (position #\- date-string :from-end t))))
          (day (parse-integer 
                (subseq date-string 
                        (+ (position #\- date-string :from-end t) 1)
                        (length date-string))))
          (hour (parse-integer 
                 (subseq time-string 
                         0 
                         (position #\: time-string))))
          (minute (parse-integer 
                   (subseq time-string 
                           (+ (position #\: time-string) 1) 
                           (length time-string)))))
      (values year month day hour minute))))
