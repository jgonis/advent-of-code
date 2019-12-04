(in-package :advent-of-code-2019)
(defun problem3-1 (input-path)
  (let* ((inputs (p3-parsing-helper input-path))
         (line-segments (create-line-segments inputs))
         (closest-intersection (find-closest-intersection line-segments)))
    (manhattan-distance closest-intersection)))

(defun problem3-2 (input-path)
  (let* ((input (p3-parsing-helper input-path)))
    2))

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
            (push (make-instance 'cl-geometry:line-segment 
                                 :start (make-instance '2d-geometry:point
                                                       :x (elt pt1 0)
                                                       :y (elt pt1 1))
                                 :end (make-instance '2d-geometry:point
                                                     :x (elt pt2 0)
                                                     :y (elt pt2 1)))
                  current-wire)
            (setf start-position pt2)))
        (setf current-wire (nreverse current-wire))
        (push current-wire line-segments)))
    line-segments))

(defun find-closest-intersection (line-segments)
  (let* ((current-closest (make-instance '2d-geometry:point 
                                         :x most-positive-fixnum
                                         :y most-positive-fixnum))
         (first-wire (elt line-segments 0))
         (second-wire (elt line-segments 1))
         (wire-1-steps 0)
         (wire-2-steps 0))
    (dolist (line-segment second-wire)
      (setf wire-2-steps 0)
      (dolist (comparison-segment first-wire)
        (let ((overlap (cl-geometry:line-segments-intersection-segment 
                        line-segment comparison-segment))
              (intersect (cl-geometry:line-segments-intersection-point
                          line-segment comparison-segment))
              (current-dist (manhattan-distance current-closest)))
          (let ((int-point (check-for-intersection overlap intersect)))
            (cond ((not (null int-point))
                   (if (< (manhattan-distance int-point) current-dist)
                       (progn
                         (setf current-closest int-point)))))))))
    current-closest))

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

(defgeneric is-origin (pt))
(defmethod is-origin ((pt 2d-geometry:point))
  (and (= 0 (2d-geometry:x pt)) (= 0 (2d-geometry:y pt))))
(defgeneric is-not-origin (pt))
(defmethod is-not-origin ((pt 2d-geometry:point))
  (not (is-origin pt)))

(defgeneric manhattan-distance (pt))
(defmethod manhattan-distance ((pt 2d-geometry:point))
  (+ (abs (2d-geometry:x pt)) (abs (2d-geometry:y pt))))

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
