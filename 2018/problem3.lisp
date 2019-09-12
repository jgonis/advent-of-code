(in-package :advent-of-code-2018)
(defun problem3-1 (input-path)
  (let ((input (aoc-utils:input->list input-path))
        (coord-hash (make-hash-table :test 'equal))
        (contested-count 0))
    (dolist (line input)
      (parse-p3 line coord-hash #'fabric-coord-func))
    (with-hash-table-iterator (next coord-hash)
      (loop (multiple-value-bind (more? key value) (next)
              (unless more? (return '()))
              (if (and (consp key) (>= value 2))
                  (setf contested-count (+ contested-count 1))))))
    contested-count))

(defun problem3-2 (input-path)
  (let ((input (aoc-utils:input->list input-path))
        (coord-hash (make-hash-table :test 'equal))
        (result 0))
    (dolist (line input)
      (parse-p3 line coord-hash #'fabric-id-coord-func))
    (with-hash-table-iterator (next coord-hash)
      (loop (multiple-value-bind (more? key value) (next)
              (unless more? (return '()))
              (if (and (stringp key) value)
                  (setf result key)))))
    result))

(defun parse-p3 (line coord-hash set-fabric-coord-func)
  (let* ((claim-id (aoc-utils:trim-spaces (subseq line
                                                  (+ 1 (position #\# line))
                                                  (position #\Space line))))
         (start-coords-string (aoc-utils:trim-spaces (subseq line
                                                             (+ 1 (position #\@ line))
                                                             (position #\: line))))
         (start-size-string (aoc-utils:trim-spaces (subseq line
                                                           (+ 1 (position #\: line))
                                                           (length line))))
         (start-x (parse-integer (subseq start-coords-string
                                         0
                                         (position #\, start-coords-string))))
         (start-y (parse-integer (subseq start-coords-string
                                         (+ 1 (position #\, start-coords-string))
                                         (length start-coords-string))))
         (width (parse-integer (subseq start-size-string
                                       0
                                       (position #\x start-size-string))))
         (height (parse-integer (subseq start-size-string
                                        (+ 1 (position #\x start-size-string))
                                        (length start-size-string)))))
    (setf (gethash claim-id coord-hash) t)
    (dotimes (x width)
      (dotimes (y height)
        (let ((x-coord (+ x start-x))
              (y-coord (+ y start-y)))
          (multiple-value-bind (value present?) (gethash (cons x-coord
                                                               y-coord)
                                                         coord-hash)
            (funcall set-fabric-coord-func
                     x-coord
                     y-coord
                     claim-id
                     value
                     present?
                     coord-hash)))))))

(defun fabric-coord-func (x-coord y-coord claim-id value present? coord-hash)
  (declare (ignore claim-id))
  (cond ((null present?) (setf (gethash (cons x-coord
                                              y-coord)
                                        coord-hash)
                               1))
        (t (setf (gethash (cons x-coord
                                y-coord)
                          coord-hash)
                 (+ value 1)))))

(defun fabric-id-coord-func (x-coord y-coord claim-id value present? coord-hash)
    (cond ((null present?) (setf (gethash (cons x-coord
                                              y-coord)
                                        coord-hash)
                               (cons claim-id 1)))
        (t (setf (gethash (cons x-coord
                                y-coord)
                          coord-hash)
                 (cons (car value) (+ (cdr  value) 1)))
           (setf (gethash (car value) coord-hash) nil)
           (setf (gethash claim-id coord-hash) nil))))
