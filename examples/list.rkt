#lang racket/base

(module+ test
  (require rackcheck
           rackunit)

  (define-property prop-list-reverse
    ([xs (gen:list gen:natural)])
    (check-equal? (reverse (reverse xs)) xs))

  (check-property prop-list-reverse))
