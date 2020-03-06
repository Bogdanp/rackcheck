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
           racket/match)

  (define gen:actions
    (gen:list
     (gen:amb
      (gen:tuple (gen:const 'push) gen:natural)
      (gen:tuple (gen:const 'pop))
      (gen:tuple (gen:const 'size)))))

  (define (interpret-ring r a)
    (match a
      [(list 'new cap) (values (make-ring cap) (void))]
      [(list 'push v)  (values r (ring-push! r v))]
      [(list 'pop)     (values r (ring-pop! r))]
      [(list 'size)    (values r (ring-size r))]))

  (struct model (cap xs)
    #:transparent)

  (define/match (interpret-model s a)
    [(_ (list 'new cap))
     (values (model cap null) (void))]

    [((model cap xs) (list 'push v))
     (values (model cap (take (cons v xs)
                              (min (add1 (length xs)) cap)))
             (void))]

    [((model cap (list)) (list 'pop))
     (values (model cap null) #f)]

    [((model cap xs) (list 'pop))
     (values (model cap (cdr xs)) (car xs))]

    [((model _ xs) (list 'size))
     (values s (length xs))])

  (define-property ring-state
    ([cap (gen:integer-in 1 100)]
     [actions gen:actions])
    (for/fold ([s #f]
               [r #f]
               [res #t]
               #:result res)
              ([a (cons (list 'new cap) actions)]
               #:when res)
      (define-values (s* expected)
        (interpret-model s a))
      (define-values (r* got)
        (interpret-ring r a))
      (values s*
              r*
              (and (= (length (model-xs s*)) (ring-size r*))
                   (equal? got expected)))))

  (check-property
   (make-config #:tests 1000)
   ring-state))
