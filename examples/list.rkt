#lang racket/base

(module+ test
  (require rackcheck)

  (define-property prop-list-reverse
    ([xs (gen:list gen:natural)])
    (equal? (reverse (reverse xs)) xs))

  (check-property prop-list-reverse))
