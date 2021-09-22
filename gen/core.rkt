#lang racket/base

(require racket/contract
         racket/function
         racket/match
         racket/promise
         racket/random)

(provide
 exn:fail:gen?
 exn:fail:gen:exhausted?

 gen?
 make-gen
 shrink-tree?
 make-shrink-tree
 shrink-tree-val
 shrink-tree-shrinks
 generator/c
 sample
 shrink

 gen:const
 gen:map
 gen:bind
 ;gen:filter
 gen:choice
 gen:sized
 gen:resize
 gen:scale
 gen:no-shrink)

(struct exn:fail:gen exn:fail ())
(struct exn:fail:gen:exhausted exn:fail:gen ())

(struct shrink-tree (val shrinks))

(define/contract (make-shrink-tree val [shrink (const '())])
  (->* (any/c) ((-> any/c (listof any/c))) shrink-tree?)
  (shrink-tree
   val
   (lazy (map (lambda (v) (make-shrink-tree v shrink))
              (shrink val)))))

(define (shrink-tree-map f st)
  (match-let ([(shrink-tree val shrinks) st])
    (shrink-tree
     (f val)
     (lazy (map (curry shrink-tree-map f)
                (force shrinks))))))

(define (shrink-tree-join st)
  (match-let ([(shrink-tree (shrink-tree inner-val inner-shrinks) outer-shrinks) st])
    (shrink-tree
     inner-val
     (lazy (append (map shrink-tree-join (force outer-shrinks))
                   (force inner-shrinks))))))

(define generator/c
  (-> pseudo-random-generator? exact-nonnegative-integer? shrink-tree?))

(struct gen (f)
  #:property prop:procedure (struct-field-index f))

(define/contract (make-gen f)
  (-> generator/c gen?)
  (gen f))

(define/contract (sample g [n 10] [rng (current-pseudo-random-generator)])
  (->* (gen?) (exact-positive-integer? pseudo-random-generator?) (listof any/c))
  (for/list ([s (in-range n)])
    (shrink-tree-val (g rng (expt s 2)))))

; it would be pretty neat to have a visual program for exploring shrink trees
(define/contract (shrink g [size 30] [n 4] [depth 8] [rng (current-pseudo-random-generator)])
  (->* (gen?) (exact-positive-integer?
               exact-positive-integer?
               exact-positive-integer?
               pseudo-random-generator?)
       (values any/c (listof (listof any/c))))
  (match-let ([(shrink-tree val shrinks) (g rng size)])
    (values
     val
     (let* ([shrinks (force shrinks)]
            [starts (random-sample shrinks (min n (length shrinks)) rng #:replacement? #f)])
       (for/list ([st starts])
         (get-shrinks st depth rng))))))

(define (get-shrinks st depth rng)
  (if (zero? depth)
      '()
      (match-let ([(shrink-tree val shrinks) st])
        (let ([shrinks (force shrinks)])
          (if (null? shrinks)
              (list val)
              (cons val (get-shrinks (random-ref shrinks rng) (sub1 depth) rng)))))))

(define (gen:const v)
  (gen
   (lambda (_rng _size)
     (shrink-tree v (lazy '())))))

(define/contract (gen:map g f)
  (-> gen? (-> any/c any/c) gen?)
  (gen
   (lambda (rng size)
     (shrink-tree-map f (g rng size)))))

(define/contract (gen:bind g h)
  (-> gen? (-> any/c gen?) gen?)
  (gen
   (lambda (rng size)
     (let ([g-st (g rng size)])
       (shrink-tree-join
        (shrink-tree-map
         (λ (val) ((h val) rng size))
         g-st))))))

#;(define/contract (gen:filter g p [max-attempts 1000])
  (->* (gen? (-> any/c boolean?))
       ((or/c exact-positive-integer? +inf.0))
       gen?)
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
  (-> gen? (-> exact-nonnegative-integer? exact-positive-integer?) gen?)
  (gen:sized
   (lambda (size)
     (gen:resize g (f size)))))

(define/contract (gen:no-shrink g)
  (-> gen? gen?)
  (gen
   (lambda (rng size)
     (shrink-tree (shrink-tree-val (g rng size)) (lazy '())))))

(module+ private
  (provide gen shrink-tree))

(module+ test
  (require rackunit)

  (check-equal? (sample (gen:const 1) 1)
                '(1))

  (check-equal? (sample (gen:const 1) 5)
                '(1 1 1 1 1))

  (check-equal? (sample (gen:map (gen:const 1) add1) 1)
                '(2))

  (check-equal? (sample (gen:bind (gen:const 1)
                                  (lambda (v)
                                    (gen:const (add1 v))))
                        1)
                '(2))

  #;(check-equal? (sample (gen:filter (gen:const 1)
                                    number?)
                        1)
                '(1))

  #;(check-exn
   exn:fail:gen?
   (lambda ()
     (sample (gen:filter (gen:const 1)
                         (lambda (v)
                           (eqv? v 2)))))))
