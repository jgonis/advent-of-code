(in-package :aoc-utils)
(defclass p4-date-time () 
  ((year :reader year)
   (month :reader month)
   (day :reader day)
   (hour :reader hour)
   (minute :reader minute)))

(defgeneric (setf year) (year date-time))
(defmethod (setf year) ((year integer) (date-time p4-date-time))
  (setf (slot-value date-time 'year) year))

(defgeneric (setf month) (month date-time))
(defmethod (setf month) ((month integer) (date-time p4-date-time))
  (setf (slot-value date-time 'month) month))

(defgeneric (setf day) (day date-time))
(defmethod (setf day) ((day integer) (date-time p4-date-time))
  (setf (slot-value date-time 'day) day))

(defgeneric (setf hour) (hour date-time))
(defmethod (setf hour) ((hour integer) (date-time p4-date-time))
  (setf (slot-value date-time 'hour) hour))

(defgeneric (setf minute) (minute date-time))
(defmethod (setf minute) ((minute integer) (date-time p4-date-time))
  (setf (slot-value date-time 'minute) minute))

(defmethod initialize-instance :after ((date-time p4-date-time) &key date-time-string)
  (multiple-value-bind (y mon d h min) (parse-time-string date-time-string)
    (setf (year date-time) y)
    (setf (month date-time) mon)
    (setf (day date-time) d)
    (setf (hour date-time) h)
    (setf (minute date-time) min)))

(defmethod print-object ((date-time-object p4-date-time) stream)
  (format stream "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d~%" 
          (if (slot-boundp date-time-object 'year) 
              (year date-time-object)
              "No year set") 
          (if (slot-boundp date-time-object 'month) 
              (month date-time-object)
              "No month set") 
          (if (slot-boundp date-time-object 'day) 
              (day date-time-object)
              "No day set") 
          (if (slot-boundp date-time-object 'hour) 
              (hour date-time-object)
              "No hour set")
          (if (slot-boundp date-time-object 'minute) 
              (minute date-time-object)
              "No minute set")))

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
          (month (parse-integer (subseq date-string 
                                        (+ (position #\- date-string) 1) 
                                        (position #\- date-string :from-end t))))
          (day (parse-integer (subseq date-string 
                                      (+ (position #\- date-string :from-end t) 1)
                                      (length date-string))))
          (hour (parse-integer (subseq time-string 
                                       0 
                                       (position #\: time-string))))
          (minute (parse-integer (subseq time-string 
                                         (+ (position #\: time-string) 1) 
                                         (length time-string)))))
      (values year month day hour minute))))
