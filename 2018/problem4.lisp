(in-package :advent-of-code-2018)
(defun problem4-1 (input-path)
  (let* ((input-lines (aoc-utils:input->list input-path))
         (sorted-lines (sort input-lines (lambda (line1 line2) 
                                           (let* ((date-string-1 (subseq line1 1 (position #\] line1)))
                                                  (date-string-2 (subseq line2 1 (position #\] line2)))
                                                  (date-obj1 (aoc-utils:make-date-time date-string-1))
                                                  (date-obj2 (aoc-utils:make-date-time date-string-2)))
                                             (aoc-utils:less-than date-obj1 date-obj2)))))
         (parsed-shifts (make-hash-table)))
    (let ((current-state (make-instance 'initial-state)))
      (dolist (line sorted-lines)
        (let* ((input (make-input line))
               (next-state (handle-input current-state 
                                         input
                                         parsed-shifts)))
          (setf current-state next-state)))
      (handle-input current-state 
                    (make-instance 'eof-input)
                    parsed-shifts))
    (format t "~A~%" parsed-shifts)))

(defclass shift-record-state () ())
;;  ((guard-number :initarg :guard-number 
;;                 :accessor guard-number)
;;   (shift-date :initarg :shift-date 
;;               :accessor shift-date)
;;   (asleep-awake-log :initarg :log 
;;                     :accessor asleep-awake-log)
;;   (state-start-minute :initarg :start-minute
;;                       :accessor start-minute)))
(defclass guard-state (shift-record-state) ())
(defclass asleep-state (shift-record-state) ())
(defclass awake-state (shift-record-state) ())
(defclass initial-state () ())

(defgeneric handle-input (state input shift-collection))
(defmethod handle-input ((state initial-state) 
                         input
                         shift-collection)
  (cond ((typep input 'guard-input) 
         (format t "Creating Guard ~A State~%" (guard-id input))
         (make-instance 'guard-state))
        ((typep input 'eof-input)
         (format t "Empty file of input~%"))
;;                        :guard-number (guard-number input)
;;                        :shift-date (date-time input)
;;                        :state-start-minute 0
;;                        :log (make-list 60 :initial-element #\.))
         (t (error "Unexpected input type for the initial state: ~A" input))))

(defmethod handle-input ((state asleep-state)
                         input
                         shift-collection)
  (cond ((typep input 'guard-input)
         (format t "Ending sleep, starting new shift for guard ~A~%" (guard-id input))
         (make-instance 'guard-state))
        ((typep input 'wake-up-input)
         (format t "creating awake-state~%")
         (make-instance 'awake-state))
        ((typep input 'eof-input)
         (format t "done parsing!~%"))
        (t (error "Unexpected input type for asleep-state: ~A" input))))

(defmethod handle-input ((state awake-state)
                         input
                         shift-collection)
  (cond ((typep input 'guard-input)
         (format t "Ending shift awake, starting new shift for guard ~A~%" (guard-id input))
         (make-instance 'guard-state))
        ((typep input 'fall-asleep-input)
         (format t "falling asleep now~%")
         (make-instance 'asleep-state))
        ((typep input 'eof-input)
         (format t "done parsing!~%"))
        (t (error "Unexpected input type for awake-state: ~A" input))))

(defmethod handle-input ((state guard-state) 
                         input
                         shift-collection)
  (cond ((typep input 'guard-input)
         (format t "Finished guard-shift, starting new shift for guard ~A~%"(guard-id input))
         (make-instance 'guard-state))
        ((typep input 'fall-asleep-input)
         (format t "falling asleep~%")
         (make-instance 'asleep-state))
        ((typep input 'eof-input)
         (format t "done parsing!~%"))
        (t (error "Unexpected input type for guard-state: ~A" input))))

  ;; (cond ((typep input 'wake-up-input) 
  ;;        (let ((next-state-start-minute (aoc-utils:minute (date-time input)))) 
  ;;              (make-instance 'asleep-state
  ;;                           :guard-number (guard-number state)
  ;;                           :shift-date (shift-date state)
  ;;                           :start-minute next-start-start-minute)
  ;;                           :log (update-log log 
  ;;                                            (start-minute state) 
  ;;                                            next-state-start-minute 
  ;;                                            :awake)))
  ;;       ((typep input 'fall-asleep-input) 
  ;;        (let ((next-state-start-minute (aoc-utils:minute (date-time input))))
  ;;          (make-instance 'awake-state
  ;;                         :guard-number (guard-number state)
  ;;                         :shift-date (shift-date state)
  ;;                         :start-minute next-state-start-minute
  ;;                         :log (update-log log
  ;;                                          (start-minute state)
  ;;                                          next-state-start-minute
  ;;                                          :asleep))))))


(defclass p4-input ()
  ((date-time :initarg :date-time :accessor date-time)))
(defclass guard-input (p4-input) 
  ((guard-id :initarg :guard-id :accessor guard-id)))
(defclass fall-asleep-input (p4-input) 
  ())
(defclass wake-up-input (p4-input) 
  ())
(defclass eof-input () ())

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
