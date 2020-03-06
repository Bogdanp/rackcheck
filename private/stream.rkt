#lang racket/base

(require racket/stream)

(provide
 (rename-out [make-unroll stream-unroll])
 stream-dedupe)

(define canary
  (let ()
    (struct canary ())
    (canary)))

(struct unroll (next tail)
  #:methods gen:stream
  [(define (stream-empty? s)
     (eq? (unroll-next s) canary))

   (define (stream-first s)
     (unroll-next s))

   (define (stream-rest s)
     (make-unroll (unroll-tail s)))])

(define (make-unroll ss)
  (define-values (next tail)
    (let loop ([ss ss])
      (cond
        [(stream-empty? ss)
         (values canary empty-stream)]

        [(stream-empty? (stream-first ss))
         (loop (stream-rest ss))]

        [else
         (define head (stream-first ss))
         (values (stream-first head)
                 (stream-cons
                  (stream-rest head)
                  (stream-rest ss)))])))

  (unroll next tail))

(module+ test
  (require rackunit)

  (define (v n)
    (begin0 n
      (printf "~a\n" n)))

  (define s
    (for/stream ([n (in-range 5)])
      (stream
       (v n)
       (v (* n 2))
       (v (expt 2 n)))))

  (check-equal?
   (stream->list (make-unroll s))
   '(0 0 1 1 2 2 2 4 4 3 6 8 4 8 16)))

(define (stream-dedupe s)
  (define seen (make-hash))
  (stream-filter
   (lambda (v)
     (begin0 (not (hash-has-key? seen v))
       (hash-set! seen v #t)))
   s))

(module+ test
  (check-equal? (stream->list (stream-dedupe (stream 1 2 3 3 0 5 1 2)))
                '(1 2 3 0 5)))
