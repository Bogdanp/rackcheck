#lang info

(define collection "rackcheck")
(define deps '("base"
               "rackunit-lib"
               "rackcheck-lib"))
(define build-deps '("racket-doc"
                     "rackunit-doc"
                     "rackunit-lib"
                     "scribble-lib"))
(define update-implies '("rackcheck-lib"))
(define scribblings '(("rackcheck.scrbl")))
(define compile-omit-paths '("examples"))
