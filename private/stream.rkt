#lang racket/base

(require racket/stream)

(provide
 (rename-out [make-flattened-stream stream-flatten])
 stream-dedupe)

(define done (gensym 'done))

(struct flattened-stream (next streams)
  #:methods gen:stream
  [(define (stream-empty? s)
     (eq? (flattened-stream-next s) done))

   (define (stream-first s)
     ((flattened-stream-next s)))

   (define (stream-rest s)
     (flattened-stream* (flattened-stream-streams s)))])

(define (flattened-stream* streams)
  (cond
    [(null? streams)
     (flattened-stream done null)]

    [(stream-empty? (car streams))
     (make-flattened-stream (cdr streams))]

    [else
     (define head (car streams))
     (flattened-stream
      (lambda ()
        (stream-first head))
      (cons
       (stream-rest head)
       (cdr streams)))]))

(define (make-flattened-stream streams)
  (cond
    [(stream-empty? streams)
     (flattened-stream done null)]

    [else
     (define head (stream-first streams))
     (define tail (stream-rest streams))
     (flattened-stream* (cons head tail))]))

(module+ test
  (require rackunit)

  (define s
    (for/stream ([n (in-range 5)])
      (stream
       n
       (* n 2)
       (expt 2 n))))

  (check-equal?
   (stream->list (make-flattened-stream s))
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
