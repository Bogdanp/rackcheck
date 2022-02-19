#lang racket/base

;; Implements the Coin example from John Hughes' talk at LambdaDays
;; 19, "Building on developers' intuitions to create effective
;; property-based tests".

(struct coin (n)
  #:transparent)

(define MAX
  1000000)

(define (make-coin n)
  (and (valid-coin-amount? n) (coin n)))

(define (coin+ a b)
  (make-coin (+ (coin-n a)
                (coin-n b))))

(define (valid-coin-amount? n)
  (<= 0 n MAX))

(module+ test
  (require rackcheck
           racket/match
           rackunit)

  (define gen:coin
    (gen:let ([n (gen:resize gen:natural 3)]
              [amt (gen:choice
                    (gen:integer-in 0 (add1 n))
                    (gen:integer-in (- MAX (add1 n)) MAX)
                    (gen:integer-in -100 (add1 MAX)))])
      (coin amt)))

  (check-property
   (make-config #:tests 10000)
   (property ([a gen:coin]
              [b gen:coin])
     (define c (coin+ a b))
     (define n (+ (coin-n a)
                  (coin-n b)))
     (label!
      (cond
        [(< (abs (- n MAX)) 3) "boundary"]
        [(<= n MAX)            "normal"]
        [else                  "overflow"]))
     (if (valid-coin-amount? n)
         (check-match c (coin (== n)))
         (check-false c)))))
