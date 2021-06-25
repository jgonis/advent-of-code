(defsystem :advent-of-code
  :description "Advent of Code"
  :version "0.0.1"
  :author "Jeff Gonis <jeffgonis@fastmail.com>"
  :licence "AGPL 3.0"
  :depends-on (:alexandria 
               :local-time
               :cl-utilities
               :cl-geometry)
  :components ((:file "packages")
               (:module "utils"
                        :depends-on ("packages")
                        :serial t
                        :components ((:file "utils")
                                     (:file "primesAndFactors")
                                     (:file "datetime")))
	       (:module "2015"
		:depends-on ("packages"
			     utils)
		:serial t
		:components ((:file "aoc2015")))
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
                                     (:file "problem4")))
               (:module "2019"
                        :depends-on ("packages"
                                     utils)
                        :serial t
                        :components ((:file "intcode")
                                     (:file "problem1")
                                     (:file "problem2")
                                     (:file "problem3")))))

(defsystem :advent-of-code/tests
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
		:components ((:file "tests")))

               (:module "utils/tests"
		:serial t
		:components ((:file "utiltests")))

	       (:module "2018/tests"
		:serial t
		:components ((:file "p4tests")))

	       (:module "2019/tests"
		:serial t
		:components ((:file "2019tests")))

	       (:module "2015/tests"
		:serial t
		:components ((:file "2015tests"))))
  :perform (asdf:test-op 
            (op system)
            (funcall (read-from-string 
                      "advent-of-code.tests:run-tests"))))
