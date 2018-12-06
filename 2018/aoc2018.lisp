(in-package :advent-of-code-2018)
(defun problem1-1 (input-path) 
  (let ((freq 0))
    (with-open-file (in-stream input-path)
      (do ((line (read-line in-stream) (read-line in-stream nil nil)))
          ((null line) freq)
        (setf freq (+ freq (parse-integer line)))))))


(defun problem1-2 (input-path)
  (let ((freq-offsets '()))
    (with-open-file (in-stream input-path)
      (do ((line (read-line in-stream) (read-line in-stream nil nil)))
          ((null line) nil)
        (push (parse-integer line) freq-offsets)))
    (setf freq-offsets (reverse freq-offsets))
    (setf (cdr (last freq-offsets)) freq-offsets)
    (problem1-2helper freq-offsets)))

(defun problem1-2helper (offset-list)
  (let ((calculated-freqs (make-hash-table))
        (current-freq 0))
    (dolist (offset offset-list)
      (setf current-freq (+ offset current-freq))
      (multiple-value-bind (value present) (gethash current-freq 
                                                    calculated-freqs)
        (cond (present (return current-freq))
              (t (setf (gethash current-freq 
                                calculated-freqs) 
                       current-freq)))))))

(defun problem2-1 (input-path)
  (let ((two-count 0)
        (three-count 0))
    (with-open-file (in-stream input-path)
      (do ((line (read-line in-stream) 
                 (read-line in-stream 
                            nil 
                            nil))) 
          ((null line))
        (let* ((letters (map 'list 
                             (lambda (x) x) 
                             line))
               (letter-hash (count-letters-in-word letters)))
          (if (contains-double-char letter-hash)
              (setf two-count 
                    (+ two-count 1)))
          (if (contains-triple-char letter-hash)
              (setf three-count 
                    (+ three-count 1))))))
    (* two-count three-count)))

(defun contains-double-char (letter-hash)
  (contains-char-count letter-hash 2))

(defun contains-triple-char (letter-hash)
  (contains-char-count letter-hash 3))

(defun contains-char-count (letter-hash count)
  (with-hash-table-iterator (next letter-hash)
    (loop (multiple-value-bind (more? key value) (next)
            (unless more? (return '()))
            (if (= value count)
                (return t))))))

(defun count-letters-in-word (letter-list)
  (let ((letter-hash (make-hash-table)))
    (mapcar (lambda (letter)
              (multiple-value-bind (value present) (gethash letter 
                                                            letter-hash)
                (cond ((null present) (setf (gethash letter 
                                                     letter-hash) 1))
                      (t (setf (gethash letter 
                                        letter-hash) (+ value 1)))))) 
            letter-list)
    letter-hash))

(defun problem2-2 (input-path)
  (let ((box-ids '()))
    (with-open-file (in-stream input-path)
      (do ((line (read-line in-stream) 
                 (read-line in-stream 
                            nil 
                            nil))) 
          ((null line))
        (push line box-ids)))
    (setf box-ids (nreverse box-ids))
    (let ((id-length (length (first box-ids)))))))

(defun strings-have-single-mismatch (string1 string2)
  (let ((mismatch-forward (mismatch string1 string2))
        (mismatch-backward (- (mismatch string1 string2 :from-end t) 1)))
    (= mismatch-forward mismatch-backward)))
  
