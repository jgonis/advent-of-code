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
    (nreverse input)))

(defun list->output (lst output-path)
  (with-open-file (out-stream output-path 
                              :direction :output 
                              :if-exists :supersede 
                              :if-does-not-exist :create)
    (dolist (elem lst)
      (format out-stream "~A~%" elem))))

(defun trim-spaces (in-string)
  (string-trim '(#\Space) in-string))

(defun extract-string-between ()
  (* 2 2))
