#lang info

(define collection "rackcheck")
(define deps '("base"
               "rackcheck-lib"))
(define build-deps '("racket-doc"
                     "rackunit-doc"
                     "scribble-lib"))
(define update-implies '("rackcheck-lib"))
(define scribblings '(("rackcheck.scrbl")))
