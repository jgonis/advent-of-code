(in-package :advent-of-code-2018)
(defun problem4-1 (input-path)
  (let* ((input-lines (aoc-utils:input->list input-path))
         (sorted-lines (sort input-lines (lambda (line1 line2) 
                                           (let* ((date-string-1 (subseq line1 1 (position #\] line1)))
                                                  (date-string-2 (subseq line2 1 (position #\] line2)))
                                                  (date-obj1 (aoc-utils:make-date-time date-string-1))
                                                  (date-obj2 (aoc-utils:make-date-time date-string-2)))
                                             (local-time:timestamp< date-obj1 date-obj2)))))
         (parsed-shifts (make-hash-table)))
    (let ((current-state (make-instance 'initial-state)))
      (dolist (line sorted-lines)
        (let* ((input (make-input line))
               (next-state (handle-input current-state 
                                         input
                                         parsed-shifts)))
          (let ((dt (date-time input)))
            (if (= (local-time:timestamp-hour dt) 23)
                (progn 
                  (format t "~A:~A mon: ~A day: ~A~%" 
                          (local-time:timestamp-hour dt)
                          (local-time:timestamp-minute dt)
                          (local-time:timestamp-month dt)
                          (local-time:timestamp-day dt))
                  (setf (date-time input) (local-time:adjust-timestamp dt 
                                            (offset :day 1) 
                                            (set :hour 0) 
                                            (set :minute 0)))
                  (format t "~A:~A mon: ~A day: ~A~%~%" 
                          (local-time:timestamp-hour (date-time input))
                          (local-time:timestamp-minute (date-time input))
                          (local-time:timestamp-month (date-time input))
                          (local-time:timestamp-day (date-time input))))))
          (setf current-state next-state)))
      (handle-input current-state 
                    (make-instance 'eof-input)
                    parsed-shifts))
    (format t "~A~%" parsed-shifts)))

(defclass shift-record-state ()
  ((guard-number :initarg :guard-number 
                 :accessor guard-number)
   (shift-date :initarg :shift-date 
               :accessor shift-date)
   (asleep-awake-log :initarg :log 
                     :accessor asleep-awake-log)
   (state-start-minute :initarg :start-minute
                       :accessor start-minute)))
(defclass guard-state (shift-record-state) ())
(defclass asleep-state (shift-record-state) ())
(defclass awake-state (shift-record-state) ())
(defclass initial-state () ())

(defclass p4-input ()
  ((date-time :initarg :date-time :accessor date-time)))
(defclass guard-input (p4-input) 
  ((guard-id :initarg :guard-id :accessor guard-id)))
(defclass fall-asleep-input (p4-input) 
  ())
(defclass wake-up-input (p4-input) 
  ())
(defclass eof-input () ())

(defgeneric handle-input (state input shift-collection))
(defmethod handle-input ((state initial-state) 
                         (input guard-input)
                         shift-collection)
  (make-instance 'guard-state))

(defmethod handle-input ((state initial-state) 
                         (input eof-input)
                         shift-collection)
  (format t "Empty file of input~%"))

(defmethod handle-input ((state asleep-state)
                         (input guard-input)
                         shift-collection)
  (make-instance 'guard-state))

(defmethod handle-input ((state asleep-state)
                         (input wake-up-input)
                         shift-collection)
  (make-instance 'awake-state))

(defmethod handle-input ((state asleep-state)
                         (input eof-input)
                         shift-collection)
  (format t "done parsing!~%"))

(defmethod handle-input ((state awake-state)
                         (input guard-input)
                         shift-collection)
  (make-instance 'guard-state))

(defmethod handle-input ((state awake-state)
                         (input fall-asleep-input)
                         shift-collection)
  (make-instance 'asleep-state))

(defmethod handle-input ((state awake-state)
                         (input eof-input)
                         shift-collection)
  (format t "done parsing!~%"))

(defmethod handle-input ((state guard-state) 
                         (input guard-input)
                         shift-collection)
  (make-instance 'guard-state))

(defmethod handle-input ((state guard-state) 
                         (input fall-asleep-input)
                         shift-collection)
  (make-instance 'asleep-state))

(defmethod handle-input ((state guard-state) 
                         (input eof-input)
                         shift-collection)
  (format t "done parsing!~%"))

(defun make-input (input-line)
  (let* ((date-string (subseq input-line 1 (position #\] input-line)))
         (date-time (aoc-utils:make-date-time date-string)))
    (cond ((search "Guard" input-line) 
           (let* ((sharp-position (position #\# input-line))
                  (id-string (subseq input-line 
                                     (1+ sharp-position) 
                                     (position #\Space 
                                               input-line 
                                               :start sharp-position)))
                  (id (parse-integer id-string)))
             (make-instance 'guard-input 
                            :date-time date-time
                            :guard-id id)))
          ((search "falls" input-line) (make-instance 'fall-asleep-input 
                                                      :date-time date-time))
          ((search "wakes" input-line) (make-instance 'wake-up-input
                                                      :date-time date-time))
          (t (error "input line is not recognized: ~A" input-line)))))


