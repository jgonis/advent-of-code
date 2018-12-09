(in-package :aoc-utils)
(defun make-growable-vector (initial-size) 
    (make-array initial-size :fill-pointer 0 :adjustable t))

(defun string->list (str)
  (map 'list (lambda (x) x) str))

(defun input->list (input-path)
  (let ((input '()))
    (with-open-file (in-stream input-path)
      (do ((line (read-line in-stream) (read-line in-stream nil nil)))
          ((null line) input)
        (push line input)))
    (reverse input)))

(defun trim-spaces (in-string)
  (string-trim '(#\Space) in-string))
