#lang info

(define version "0.0.0")
(define collection "rackcheck")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("racket-doc"
                     "rackunit-doc"
                     "rackunit-lib"
                     "scribble-lib"))
(define scribblings '(("rackcheck.scrbl")))
(define test-omit-paths '("examples"))
