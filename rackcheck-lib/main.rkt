#lang racket/base

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(reprovide "gen/base.rkt"
           "gen/syntax.rkt"
           "gen/unicode.rkt"
           "prop.rkt"
           "rackunit.rkt")
