(in-package :aoc-utils)
(defclass p4-date-time () 
  ((year :reader year)
   (month :reader month)
   (day :reader day)
   (hour :reader hour)
   (minute :reader minute)))

(defgeneric (setf year) (date-time year))
(defmethod (setf year) ((date-time p4-date-time) (year integer))
  (setf (slot-value date-time 'year) year))

(defgeneric (setf month) (date-time month))
(defmethod (setf month) ((date-time p4-date-time) (month integer))
  (setf (slot-value date-time 'month) month))

(defgeneric (setf day) (date-time day))
(defmethod (setf day) ((date-time p4-date-time) (day integer))
  (setf (slot-value date-time 'day) day))

(defgeneric (setf hour) (date-time hour))
(defmethod (setf hour) ((date-time p4-date-time) (hour integer))
  (setf (slot-value date-time 'hour) hour))

(defgeneric (setf minute) (date-time minute))
(defmethod (setf minute) ((date-time p4-date-time) (minute integer))
  (setf (slot-value date-time 'minute) minute))

(defmethod initialize-instance :after ((date-time p4-date-time) &key date-time-string)
  (setf (slot-value date-time 'year) 1983)
  (setf (slot-value date-time 'month) "05")
  (setf (slot-value date-time 'day) 20)
  (setf (slot-value date-time 'hour) 12)
  (setf (slot-value date-time 'minute) 0))

(defmethod print-object ((date-time-object p4-date-time) stream)
  (format t "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" 
          (year date-time-object) 
          (month date-time-object) 
          (day date-time-object) 
          (hour date-time-object)
          (minute date-time-object)))

(defun parse-time-string (line)
  (let ((date-string (subseq line 
                             0 
                             (position #\Space line)))
        (time-string (subseq line 
                             (+ (position #\Space line) 1) 
                             (length line))))
    
    (let ((year (subseq date-string 
                        0 
                        (position #\- date-string)))
          (month (subseq date-string 
                         (+ (position #\- date-string) 1) 
                         (position #\- date-string :from-end t)))
          (day (subseq date-string 
                       (+ (position #\- date-string :from-end t) 1)
                       (length date-string))))
      (format t "year: ~A month: ~A day: ~A~%" year month day)
      (format t "~A~%" time-string))))
