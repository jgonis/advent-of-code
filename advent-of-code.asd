(defsystem :advent-of-code
  :description "Advent of Code"
  :version "0.0.1"
  :author "Jeff Gonis <jeffgonis@fastmail.com>"
  :licence "AGPL 3.0"
  :depends-on ("alexandria")
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
                        :components ((:file "aoc2018")))))
