(in-package :advent-of-code-2019)
(defun problem3-1 (input-path)
  (let* ((inputs (p3-parsing-helper input-path))
         (line-segments (create-line-segments inputs))
         (ret-vals (find-closest-intersection line-segments))
         (closest-intersection (elt ret-vals 0)))
    (manhattan-distance closest-intersection)))

(defun problem3-2 (input-path)
  (let* ((input (p3-parsing-helper input-path))
         (line-segments (create-line-segments input))
         (ret-vals (find-closest-intersection line-segments)))
    (elt ret-vals 1)))

(defun create-line-segments (inputs)
  (let ((line-segments (list)))
    (dolist (line inputs)
      (let ((start-position (list 0 0))
            (current-wire (list)))
        (dolist (command line)
          (let ((pt1 (copy-list start-position))
                (pt2 (copy-list start-position)))
            (cond ((equal (car command) "U") (setf (elt pt2 1) 
                                                   (+ (elt pt1 1) 
                                                      (cdr command))))
                  ((equal (car command) "D") (setf (elt pt2 1)
                                                   (- (elt pt1 1)
                                                      (cdr command))))
                  ((equal (car command) "L") (setf (elt pt2 0)
                                                   (- (elt pt1 0)
                                                      (cdr command))))
                  ((equal (car command) "R") (setf (elt pt2 0)
                                                   (+ (elt pt1 0)
                                                      (cdr command)))))
            (push (line-segment-from-coords (elt pt1 0)
                                            (elt pt1 1)
                                            (elt pt2 0)
                                            (elt pt2 1))
                  current-wire)
            (setf start-position pt2)))
        (setf current-wire (nreverse current-wire))
        (push current-wire line-segments)))
    line-segments))

(defun find-closest-intersection (line-segments)
  (let* ((current-closest (make-point most-positive-fixnum most-positive-fixnum))
         (current-travel most-positive-fixnum)
         (first-wire (elt line-segments 0))
         (second-wire (elt line-segments 1))
         (wire-1-steps 0)
         (wire-2-steps 0))
    (dolist (second-wire-segment second-wire)
      (setf wire-1-steps 0)
      (dolist (first-wire-segment first-wire)
        (let ((overlap (cl-geometry:line-segments-intersection-segment 
                        second-wire-segment first-wire-segment))
              (intersect (cl-geometry:line-segments-intersection-point
                          second-wire-segment first-wire-segment))
              (current-dist (manhattan-distance current-closest)))
          (let ((int-point (check-for-intersection overlap intersect)))
            (cond ((not (null int-point))
                   (let ((intersection-travel (calculate-intersection-travel int-point
                                                                             first-wire-segment
                                                                             second-wire-segment
                                                                             wire-1-steps
                                                                             wire-2-steps)))
                     (if (< intersection-travel current-travel)
                         (setf current-travel intersection-travel)))
                   (if (< (manhattan-distance int-point) current-dist)
                       (setf current-closest int-point))))))
        (setf wire-1-steps (+ wire-1-steps (cl-geometry:line-segment-length first-wire-segment))))
      (setf wire-2-steps (+ wire-2-steps (cl-geometry:line-segment-length second-wire-segment))))
    (list current-closest current-travel)))

(defun calculate-intersection-travel (int-point 
                                      first-wire-segment 
                                      second-wire-segment 
                                      wire-1-steps 
                                      wire-2-steps)
  (let* ((wire-1-segment (line-segment-from-pts 
                         (cl-geometry:start first-wire-segment)
                         int-point))
         (wire-2-segment (line-segment-from-pts
                          (cl-geometry:start second-wire-segment)
                          int-point))
         (wire-1-travel (+ wire-1-steps
                           (cl-geometry:line-segment-length wire-1-segment)))
         (wire-2-travel (+ wire-2-steps
                           (cl-geometry:line-segment-length wire-2-segment))))
    (+ wire-1-travel wire-2-travel)))

(defun check-for-intersection (overlap intersect)
  (cond ((and (null overlap) (not (null intersect)))
         (if (and (is-not-origin intersect))
             (return-from check-for-intersection intersect)))
        ((not (null overlap)) 
         (let* ((start-pt (cl-geometry:start overlap))
                (end-pt (cl-geometry:end overlap))
                (m-dist-start (manhattan-distance start-pt))
                (m-dist-end (manhattan-distance end-pt)))
           (if (and (is-not-origin start-pt) 
                    (<= m-dist-start m-dist-end))
               (return-from check-for-intersection start-pt))
           (if (and (is-not-origin end-pt) 
                    (< m-dist-end m-dist-start))
               (return-from check-for-intersection end-pt))))
        (t '())))

(defun p3-parsing-helper (input-path)
  (let* ((input-lines (aoc-utils:input->list input-path))
         (input-strings (map 'list
                             (lambda (x)
                               (cl-utilities:split-sequence
                                #\,
                                x))
                             input-lines))
         (input-pairs (map 'list
                           (lambda (line)
                             (map 'list
                                  (lambda (elem)
                                    (let* ((direction (subseq elem 0 1))
                                           (elem-length (length elem))
                                           (distance (parse-integer 
                                                      (subseq elem 
                                                              1 
                                                              elem-length))))
                                      (cons direction distance)))
                                  line))
                           input-strings)))
    input-pairs))

(defgeneric get-start-x (ls))
(defmethod get-start-x ((ls cl-geometry:line-segment))
  (2d-geometry:x (cl-geometry:start ls)))

(defgeneric get-start-y (ls))
(defmethod get-start-y ((ls cl-geometry:line-segment))
  (2d-geometry:y (cl-geometry:start ls)))

(defgeneric get-end-x (ls))
(defmethod get-end-x ((ls cl-geometry:line-segment))
  (2d-geometry:x (cl-geometry:end ls)))

(defgeneric get-end-y (ls))
(defmethod get-end-y ((ls cl-geometry:line-segment))
  (2d-geometry:y (cl-geometry:end ls)))

(defgeneric is-origin (pt))
(defmethod is-origin ((pt 2d-geometry:point))
  (and (= 0 (2d-geometry:x pt)) (= 0 (2d-geometry:y pt))))

(defgeneric is-not-origin (pt))
(defmethod is-not-origin ((pt 2d-geometry:point))
  (not (is-origin pt)))

(defgeneric manhattan-distance (pt))
(defmethod manhattan-distance ((pt 2d-geometry:point))
  (+ (abs (2d-geometry:x pt)) (abs (2d-geometry:y pt))))

(defun line-segment-from-coords (x1 y1 x2 y2)
  (make-instance 'cl-geometry:line-segment 
                 :start (make-point x1 y1)
                 :end (make-point x2 y2)))

(defun line-segment-from-pts (start-pt end-pt)
  (make-instance 'cl-geometry:line-segment 
                 :start start-pt
                 :end end-pt))

(defun make-point (x1 y1)
  (make-instance '2d-geometry:point 
                 :x x1
                 :y y1))
