(defpackage :advent-of-code-2016
  (:nicknames :aoc-16)
  (:use :common-lisp)
  (:export :problem1))

(defpackage :advent-of-code-2017
  (:nicknames :aoc-17)
  (:use :common-lisp)
  (:export :problem1))

(defpackage :advent-of-code-2018
  (:nicknames :aoc-18)
  (:use :common-lisp)
  (:export :problem1-1
           :problem1-2
           :problem2-1
           :problem2-2
           :problem3-1
           :problem3-2
           :problem4-1
           :problem4-2
           :p4-date-time))

(defpackage :aoc-utils
  (:use :common-lisp)
  (:export :make-growable-vector
           :string->list
           :input->list
           :trim-spaces
           :is-prime?
           :generate-prime-list-of-size
           :generate-prime-list-up-to
           :generate-prime-factor-list))
