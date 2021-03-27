#lang racket/base

(require racket/stream)

(provide
 (rename-out [make-flattened-stream stream-flatten])
 stream-dedupe)

(define done (gensym 'done))

(struct flattened-stream (next current streamss)
  #:methods gen:stream
  [(define (stream-empty? s)
     (eq? (flattened-stream-next s) done))

   (define (stream-first s)
     ((flattened-stream-next s)))

   (define (stream-rest s)
     (flattened-stream*
      (flattened-stream-current s)
      (flattened-stream-streamss s)))])

(define (flattened-stream* current streamss)
  (if (stream-empty? current)
      (make-flattened-stream streamss)
      (flattened-stream
       (lambda ()
         (stream-first current))
       (stream-rest current)
       streamss)))

(define (make-flattened-stream streamss)
  (if (stream-empty? streamss)
      (flattened-stream done #f empty-stream)
      (flattened-stream*
       (stream-first streamss)
       (stream-rest streamss))))

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
