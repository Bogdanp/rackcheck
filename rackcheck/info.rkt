#lang info

(define collection "rackcheck")
(define deps '("base"
               "rackcheck-lib"))
(define build-deps '("racket-doc"
                     "rackunit-doc"
                     "rackunit-lib"
                     "scribble-lib"))
(define implies '("rackcheck-lib"))
(define scribblings '(("rackcheck.scrbl")))
