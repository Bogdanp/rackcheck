#lang racket/base

(struct point (x y)
  #:transparent)

(define (distance p1 p2)
  (define dx (- (point-x p1) (point-x p2)))
  (define dy (- (point-y p1) (point-x p2)))
  (sqrt (+ (* dx dx)
           (* dy dy))))

(module+ test
  (require rackcheck
           rackunit)

  (define gen:point
    (gen:let ([x (gen:integer-in -9999 9999)]
              [y (gen:integer-in -9999 9999)])
      (point x y)))

  (check-property
   (property ([p1 gen:point]
              [p2 gen:point])
     (check-equal? (distance p1 p2)
                   (distance p2 p1)))))
