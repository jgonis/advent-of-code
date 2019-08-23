(in-package :advent-of-code-2018)
(defun problem1-1 (input-path) 
  (let ((freq 0)
        (input (aoc-utils:input->list input-path)))
    (dolist (line input)
      (setf freq (+ freq (parse-integer line))))
    freq))

(defun problem1-2 (input-path)
  (let ((input (aoc-utils:input->list input-path)))
    (map-into input (lambda (x) (parse-integer x)) input)
    (setf (cdr (last input)) input)
    (problem1-2helper input)))

(defun problem1-2helper (offset-list)
  (let ((calculated-freqs (make-hash-table))
        (current-freq 0))
    (dolist (offset offset-list)
      (setf current-freq (+ offset current-freq))
      (multiple-value-bind (value present) (gethash current-freq 
                                                    calculated-freqs)
        (declare (ignore value))
        (cond (present (return current-freq))
              (t (setf (gethash current-freq 
                                calculated-freqs) 
                       current-freq)))))))

(defun problem2-1 (input-path)
  (let ((two-count 0)
        (three-count 0)
        (input (aoc-utils:input->list input-path)))
    (dolist (line input)
      (let* ((letters (map 'list 
                           (lambda (x) x) 
                           line))
             (letter-hash (count-letters-in-word letters)))
        (if (contains-double-char letter-hash)
            (setf two-count 
                  (+ two-count 1)))
        (if (contains-triple-char letter-hash)
            (setf three-count 
                  (+ three-count 1)))))
    (* two-count three-count)))

(defun contains-double-char (letter-hash)
  (contains-char-count letter-hash 2))

(defun contains-triple-char (letter-hash)
  (contains-char-count letter-hash 3))

(defun contains-char-count (letter-hash count)
  (with-hash-table-iterator (next letter-hash)
    (loop (multiple-value-bind (more? key value) (next)
            (declare (ignore key))
            (unless more? (return '()))
            (if (= value count)
                (return t))))))

(defun count-letters-in-word (letter-list)
  (let ((letter-hash (make-hash-table)))
    (mapcar (lambda (letter)
              (multiple-value-bind (value present) (gethash letter 
                                                            letter-hash)
                (cond ((null present) (setf (gethash letter 
                                                     letter-hash) 
                                            1))
                      (t (setf (gethash letter 
                                        letter-hash) 
                               (+ value 1)))))) 
            letter-list)
    letter-hash))

(defun problem2-2 (input-path)
  (let ((box-ids (aoc-utils:input->list input-path)))
    (setf box-ids (nreverse box-ids)) ;;After reading in all the ids, 
    (format t "~A~%"  (coerce (scan-id-list box-ids) ;; reverse them so they are in the
                              'string)))) ;;order we read them in.

(defun scan-id-list (id-list)
  (cond ((null id-list) nil)
        (t (let ((result (find-id-match-against-list (car id-list) 
                                                     (cdr id-list))))
             (cond ((null result) (scan-id-list (cdr id-list)))
                   (t result))))))
(defun find-id-match-against-list (id id-list)
  (cond ((null id-list) nil)
        ((strings-have-single-mismatch id (car id-list))
         (remove-if-not (lambda (x) (alpha-char-p x))
                        (mapcar (lambda (x y) (if (char= x y) 
                                                  x
                                                  #\Newline))
                                (aoc-utils:string->list id)
                                (aoc-utils:string->list (car id-list)))))
        (t (find-id-match-against-list id (cdr id-list)))))

(defun strings-have-single-mismatch (string1 string2)
  (let ((mismatch-forward (mismatch string1 string2))
        (mismatch-backward (- (mismatch string1 string2 :from-end t) 1)))
    (= mismatch-forward mismatch-backward)))

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

(defun problem3-2 (input-path)
  (let ((input (aoc-utils:input->list input-path))
        (coord-hash (make-hash-table :test 'equal)))
    (dolist (line input)
      (parse-p3 line coord-hash #'fabric-id-coord-func))
    (with-hash-table-iterator (next coord-hash) 
      (loop (multiple-value-bind (more? key value) (next)
              (unless more? (return '()))
              (if (and (stringp key) value)
                  (format t "~A~%" key)))))))
  
(defun problem4-1 (input-path)
  (flet ((parse-date-time (dt-string)
           (let*)))
      (let* ((input (aoc-utils:input->list input-path))
             (sorted-input ()))
        )))

(defun problem4-2 (input-path))
