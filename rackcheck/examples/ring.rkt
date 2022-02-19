#lang racket/base

(struct ring (sema vs cap [pos #:mutable] [size #:mutable])
  #:transparent)

(define (make-ring cap)
  (ring (make-semaphore 1)
        (make-vector cap #f)
        cap
        0
        0))

(define (ring-push! r v)
  (call-with-semaphore (ring-sema r)
    (lambda _
      (define vs (ring-vs r))
      (define cap (ring-cap r))
      (define pos (ring-pos r))
      (vector-set! vs pos v)
      (set-ring-pos! r (modulo (add1 pos) cap))
      (set-ring-size! r (min cap (add1 (ring-size r)))))))

(define (ring-pop! r)
  (call-with-semaphore (ring-sema r)
    (lambda _
      (cond
        [(zero? (ring-size r)) #f]
        [else
         (define vs (ring-vs r))
         (define cap (ring-cap r))
         (define pos (if (zero? (ring-pos r))
                         (sub1 cap)
                         (sub1 (ring-pos r))))
         (begin0 (vector-ref vs pos)
           (vector-set! vs pos #f)
           (set-ring-pos! r pos)
           (set-ring-size! r (sub1 (ring-size r))))]))))

(module+ test
  (require rackcheck
           racket/list
           racket/match
           rackunit)

  (struct model (r l)
    #:transparent)

  (define gen:ops
    (gen:let ([cap (gen:integer-in 1 100)]
              [ops (gen:list
                    (gen:choice
                     (gen:tuple (gen:const 'push) gen:natural)
                     (gen:tuple (gen:const 'pop))
                     (gen:tuple (gen:const 'size))))])
      (cons `(init ,cap) ops)))

  (define/match (interpret s op)
    [(_ (list 'init cap))
     (model (make-ring cap) null)]

    [((model r l) (list 'push v))
     (define l*
       (take (cons v l)
             (min (add1 (length l))
                  (ring-cap r))))
     (begin0 (model r l*)
       (ring-push! r v)
       (check-equal? (ring-size r) (length l*)))]

    [((model r (list)) (list 'pop))
     (begin0 s
       (check-false (ring-pop! r)))]

    [((model r l) (list 'pop))
     (define v (car l))
     (define l* (cdr l))
     (begin0 (model r l*)
       (check-equal? (ring-pop! r) v))]

    [((model r l) (list 'size))
     (begin0 s
       (check-equal? (ring-size r) (length l)))])

  (define-property ring-state
    ([ops gen:ops])
    (for/fold ([s #f])
              ([op (in-list ops)])
      (interpret s op)))

  (check-property ring-state))
