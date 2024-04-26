#lang racket/base

(require racket/contract/base
         racket/random
         racket/stream
         (submod "shrink-tree.rkt" private))

(provide
 exn:fail:gen?
 exn:fail:gen:exhausted?

 gen?
 generator/c
 (contract-out
  [make-gen (-> generator/c gen?)]
  [sample (->* [gen?]
               [exact-positive-integer?
                pseudo-random-generator?]
               (listof any/c))]
  [shrink (->* [gen? exact-positive-integer?]
               [pseudo-random-generator?
                #:limit (or/c #f exact-positive-integer?)
                #:max-depth (or/c #f exact-nonnegative-integer?)]
               (listof any/c))]
  [gen:const (-> any/c gen?)]
  [gen:map (-> gen? (-> any/c any/c) gen?)]
  [gen:bind (-> gen? (-> any/c gen?) gen?)]
  [gen:filter (->* [gen? (-> any/c boolean?)]
                   [(or/c exact-positive-integer? +inf.0)]
                   gen?)]
  [gen:choice (-> gen? gen? ... gen?)]
  [gen:sized (-> (-> exact-nonnegative-integer? gen?) gen?)]
  [gen:resize (-> gen? exact-nonnegative-integer? gen?)]
  [gen:scale (-> gen? (-> exact-nonnegative-integer? exact-nonnegative-integer?) gen?)]
  [gen:no-shrink (-> gen? gen?)]
  [gen:with-shrink (-> gen? (-> any/c stream?) gen?)]))

(struct exn:fail:gen exn:fail ())
(struct exn:fail:gen:exhausted exn:fail:gen ())

(define generator/c
  (-> pseudo-random-generator? exact-nonnegative-integer? shrink-tree?))

(struct gen (proc)
  #:property prop:procedure (struct-field-index proc))

(define (make-gen proc)
  (gen proc))

(define (sample g [n 10] [rng (current-pseudo-random-generator)])
  (for/list ([s (in-range n)])
    (shrink-tree-val (g rng (expt s 2)))))

(define (shrink g
                size
                [rng (current-pseudo-random-generator)]
                #:limit [limit #f]
                #:max-depth [max-depth 1])
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

(define (gen:map g f)
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

(define (gen:bind g proc)
  (gen
   (lambda (rng size)
     (define g-tree (g rng size))
     (define make-rng (make-rng-proc rng))
     (shrink-tree-join
      (shrink-tree-map
       g-tree
       (λ (val)
         ((proc val) (make-rng) size)))))))

(define (gen:filter g p [max-attempts 1000])
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
         [else (search (add1 attempts) (add1 size))])))))

(define (gen:choice . gs)
  (gen
   (lambda (rng size)
     ((random-ref gs rng) rng size))))

(define (gen:sized f)
  (gen
   (lambda (rng size)
     ((f size) rng size))))

(define (gen:resize g size)
  (gen
   (lambda (rng _size)
     (g rng size))))

(define (gen:scale g f)
  (gen:sized
   (lambda (size)
     (gen:resize g (f size)))))

(define (gen:no-shrink g)
  (gen
   (lambda (rng size)
     (shrink-tree (shrink-tree-val (g rng size)) empty-stream))))

(define (gen:with-shrink g proc)
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
