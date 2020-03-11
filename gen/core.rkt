#lang racket/base

(require racket/contract
         racket/stream
         "../private/stream.rkt")

(provide
 exn:fail:gen?
 exn:fail:gen:exhausted?

 gen?
 make-gen
 generator/c
 sample
 shrink

 gen:const
 gen:map
 gen:and-then
 gen:filter
 gen:no-shrink)

(struct exn:fail:gen exn:fail ())
(struct exn:fail:gen:exhausted exn:fail:gen ())

(define generator/c
  (-> pseudo-random-generator? exact-nonnegative-integer? (stream/c any/c)))

(struct gen (f)
  #:property prop:procedure (struct-field-index f))

(define/contract (make-gen f)
  (-> generator/c gen?)
  (gen f))

(define/contract (sample g [n 10] [rng (current-pseudo-random-generator)])
  (->* (gen?) (exact-positive-integer? pseudo-random-generator?) (listof any/c))
  (for/list ([s (in-range n)])
    (stream-first (g rng (expt s 2)))))

(define/contract (shrink g [size 30] [rng (current-pseudo-random-generator)])
  (->* (gen?) (exact-positive-integer? pseudo-random-generator?) (values any/c (listof any/c)))
  (define s (g rng size))
  (values
   (stream-first s)
   (stream->list (stream-rest s))))

(define (gen:const v)
  (gen
   (lambda (_rng _size)
     (stream v))))

(define (gen:map g f)
  (gen
   (lambda (rng size)
     (stream-map f (g rng size)))))

(define (gen:and-then g h)
  (gen
   (lambda (rng size)
     (stream-dedupe
      (stream-unroll
       (for/stream ([v (in-stream (g rng size))])
         ((h v) rng size)))))))

(define (gen:filter g p [max-attempts 1000])
  (gen
   (lambda (rng size)
     (let search ([attempts 0]
                  [size size])
       (define s (stream-filter p (g rng size)))
       (cond
         [(not (stream-empty? s)) s]

         [(= attempts max-attempts)
          (raise (exn:fail:gen:exhausted (format "exhausted after ~a attempts" attempts)
                                         (current-continuation-marks)))]

         [else
          (search (add1 attempts) (add1 size))])))))

(define (gen:no-shrink g)
  (gen
   (lambda (rng size)
     (stream (stream-first (g rng size))))))

(module+ private
  (provide gen))

(module+ test
  (require rackunit)

  (check-equal? (sample (gen:const 1) 1)
                '(1))

  (check-equal? (sample (gen:const 1) 5)
                '(1 1 1 1 1))

  (check-equal? (sample (gen:map (gen:const 1) add1) 1)
                '(2))

  (check-equal? (sample (gen:and-then (gen:const 1)
                                      (lambda (v)
                                        (gen:const (add1 v))))
                        1)
                '(2))

  (check-equal? (sample (gen:filter (gen:const 1)
                                    number?)
                        1)
                '(1))

  (check-exn
   exn:fail:gen?
   (lambda ()
     (sample (gen:filter (gen:const 1)
                         (lambda (v)
                           (eqv? v 2)))))))
