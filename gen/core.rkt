#lang racket/base

(require racket/contract
         racket/random
         racket/stream
         (submod "shrink-tree.rkt" private))

(provide
 exn:fail:gen?
 exn:fail:gen:exhausted?

 gen?
 generator/c
 make-gen
 sample
 shrink

 gen:const
 gen:map
 gen:bind
 gen:filter
 gen:choice
 gen:sized
 gen:resize
 gen:scale
 gen:no-shrink
 gen:with-shrink)

(struct exn:fail:gen exn:fail ())
(struct exn:fail:gen:exhausted exn:fail:gen ())

(define generator/c
  (-> pseudo-random-generator? exact-nonnegative-integer? shrink-tree?))

(struct gen (proc)
  #:property prop:procedure (struct-field-index proc))

(define/contract (make-gen proc)
  (-> generator/c gen?)
  (gen proc))

(define/contract (sample g [n 10] [rng (current-pseudo-random-generator)])
  (->* (gen?) (exact-positive-integer? pseudo-random-generator?) (listof any/c))
  (for/list ([s (in-range n)])
    (shrink-tree-val (g rng (expt s 2)))))

(define/contract (shrink g
                         size
                         [rng (current-pseudo-random-generator)]
                         #:limit [limit #f]
                         #:max-depth [max-depth 1])
  (->* (gen? exact-positive-integer?)
       (pseudo-random-generator?
        #:limit (or/c #f exact-positive-integer?)
        #:max-depth (or/c #f exact-nonnegative-integer?))
       (listof any/c))
  (let loop ([tree (g rng size)]
             [depth 0])
    (cons
     (shrink-tree-val tree)
     (cond
       [(and max-depth (= max-depth depth))
        (if (stream-empty? (shrink-tree-shrinks tree))
            '()
            '(...))]
       [else
        (let across ([shrinks (shrink-tree-shrinks tree)] [n 0])
          (cond
            [(stream-empty? shrinks) '()]
            [(and limit (= limit n)) '(...)]
            [else (cons (loop (stream-first shrinks) (add1 depth))
                        (across (stream-rest shrinks) (add1 n)))]))]))))

(define (gen:const v)
  (gen
   (lambda (_rng _size)
     (shrink-tree v empty-stream))))

(define/contract (gen:map g f)
  (-> gen? (-> any/c any/c) gen?)
  (gen
   (lambda (rng size)
     (shrink-tree-map (g rng size) f))))

(define (make-rng-proc rng)
  (define state
    (vector
     (random 4294967087 rng)
     (random 4294967087 rng)
     (random 4294967087 rng)
     (random 4294944443 rng)
     (random 4294944443 rng)
     (random 4294944443 rng)))
  (λ ()
    (vector->pseudo-random-generator state)))

(define/contract (gen:bind g proc)
  (-> gen? (-> any/c gen?) gen?)
  (gen
   (lambda (rng size)
     (define g-tree (g rng size))
     (define make-rng (make-rng-proc rng))
     (shrink-tree-join
      (shrink-tree-map
       g-tree
       (λ (val)
         ((proc val) (make-rng) size)))))))

(define/contract (gen:filter g p [max-attempts 1000])
  (->* (gen? (-> any/c boolean?))
       ((or/c exact-positive-integer? +inf.0))
       gen?)
  (gen
   (lambda (rng size)
     (let search ([attempts 0]
                  [size size])
       (cond
         [(shrink-tree-filter (g rng size) p)]

         [(= attempts max-attempts)
          (raise (exn:fail:gen:exhausted
                  (format "exhausted after ~a attempts" attempts)
                  (current-continuation-marks)))]

         [else
          (search (add1 attempts) (add1 size))])))))

(define/contract (gen:choice . gs)
  (-> gen? gen? ... gen?)
  (gen
   (lambda (rng size)
     ((random-ref gs rng) rng size))))

(define/contract (gen:sized f)
  (-> (-> exact-nonnegative-integer? gen?) gen?)
  (gen
   (lambda (rng size)
     ((f size) rng size))))

(define/contract (gen:resize g size)
  (-> gen? exact-nonnegative-integer? gen?)
  (gen
   (lambda (rng _size)
     (g rng size))))

(define/contract (gen:scale g f)
  (-> gen? (-> exact-nonnegative-integer? exact-nonnegative-integer?) gen?)
  (gen:sized
   (lambda (size)
     (gen:resize g (f size)))))

(define/contract (gen:no-shrink g)
  (-> gen? gen?)
  (gen
   (lambda (rng size)
     (shrink-tree (shrink-tree-val (g rng size)) empty-stream))))

(define/contract (gen:with-shrink g proc)
  (-> gen? (-> any/c stream?) gen?)
  (gen
   (lambda (rng size)
     (make-shrink-tree (shrink-tree-val (g rng size)) proc))))

(module+ private
  (provide gen))

(module+ test
  (require rackunit)

  (check-equal? (sample (gen:const 1) 1) '(1))
  (check-equal? (sample (gen:const 1) 5) '(1 1 1 1 1))
  (check-equal? (sample (gen:map (gen:const 1) add1) 1) '(2))
  (check-equal? (sample (gen:bind (gen:const 1) (λ (v) (gen:const (add1 v)))) 1) '(2))
  (check-equal? (sample (gen:filter (gen:const 1) number?) 1) '(1))

  (check-exn
   exn:fail:gen?
   (lambda ()
     (sample (gen:filter (gen:const 1)
                         (λ (v) (eqv? v 2)))))))
