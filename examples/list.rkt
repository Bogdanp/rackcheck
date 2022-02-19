#lang racket/base

(module+ test
  (require rackcheck
           rackunit)

  (define-property prop-list-reverse
    ([xs (gen:list gen:natural)])
    (check-equal? (reverse (reverse xs)) xs))

  (define-property prop-list-map-identity
    ([xs (gen:list gen:natural)])
    (check-equal? (map values xs) xs))

  (property-name prop-list-map-identity)

  (define-property prop-list-map-composition
     ([xs (gen:list gen:natural)])
     (let ([f (lambda (n) (* 3 (add1 n)))]
           [g (lambda (n) (/ n 2))])
      (check-equal? (map f (map g xs)) (map (compose f g) xs))))

   (define (test-property prop)
     (test-case (symbol->string (property-name prop))
         (check-property prop)))

  (test-property prop-list-reverse)

  (test-property prop-list-map-identity)

  (test-property prop-list-map-composition))
