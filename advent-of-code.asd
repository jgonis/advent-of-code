(defsystem :advent-of-code
  :description "Advent of Code"
  :version "0.0.1"
  :author "Jeff Gonis <jeffgonis@fastmail.com>"
  :licence "AGPL 3.0"
  :depends-on (:alexandria 
               :local-time)
  :components ((:file "packages")
               (:module "utils"
                        :depends-on ("packages")
                        :serial t
                        :components ((:file "utils")
                                     (:file "primesAndFactors")
                                     (:file "datetime")))
               (:module "2016" 
                        :depends-on ("packages" 
                                     utils)
                        :serial t 
                        :components ((:file "aoc2016")))
               (:module "2017"
                        :depends-on ("packages" 
                                     utils)
                        :serial t
                        :components ((:file "aoc2017")))
               (:module "2018"
                        :depends-on ("packages" 
                                     utils)
                        :serial t
                        :components ((:file "problem1")
                                     (:file "problem2")
                                     (:file "problem3")
                                     (:file "problem4")))))

(defsystem :advent-of-code/test
  :description "Test suite for the advent of code"
  :license "LGPL 3.0"
  :author "Jeff Gonis"
  :depends-on (:advent-of-code 
               :1am
               :cl-utilities)
  :serial t
  :components ((:file "packages.test")
               (:module "tests"
                        :serial t
                        :components ((:file "tests"))))
  :perform (asdf:test-op (op system)
             (funcall (read-from-string "advent-of-code.test:run-tests"))))
