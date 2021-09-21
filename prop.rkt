#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/match
         racket/random
         racket/stream
         "gen/syntax.rkt"
         (submod "gen/shrink-tree.rkt" private))

;; property ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (rename-out [prop? property?])
 property
 define-property)

(struct prop (name arg-ids g f))

(module+ private
  (provide (struct-out prop)))

(define-syntax (property stx)
  (syntax-parse stx
    [(_ (~optional (~or name-id:id (~seq #:name name-ex:expr)))
        ([id:id g:expr] ...)
        body ...+)
     #'(prop (~? 'name-id (~? name-ex 'unnamed))
             (list 'id ...)
             (gen:let ([id g] ...)
               (list id ...))
             (lambda (id ...)
               body ...))]))

(define-syntax (define-property stx)
  (syntax-parse stx
    [(_ name:id binds body ...+)
     #'(define name
         (property name binds body ...))]))

(module+ test
  (require "gen/base.rkt")

  (define prop-addition-commutes
    (property ([a gen:natural]
               [b gen:natural])
      (= (+ a b)
         (+ b a))))

  (define prop-addition-is-multiplication
    (property ([a gen:natural]
               [b gen:natural])
      (= (+ a b)
         (* a b)))))


;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 config?
 make-config)

(struct config (seed tests size deadline))

(define/contract (make-config #:seed [seed (make-random-seed)]
                              #:tests [tests 100]
                              #:size [size (lambda (n)
                                             (expt (sub1 n) 2))]
                              #:deadline [deadline (+ (current-inexact-milliseconds) (* 60 1000))])
  (->* ()
       (#:seed (integer-in 0 (sub1 (expt 2 31)))
        #:tests exact-positive-integer?
        #:size (-> exact-positive-integer? exact-nonnegative-integer?)
        #:deadline (>=/c 0))
       config?)
  (config seed tests size deadline))

(module+ private
  (provide (struct-out config)))


;; result ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct result (config prop labels tests-run status args args/smallest e)
  #:transparent)

(define (make-result config prop labels tests-run status [args #f] [args/smallest #f] [exception #f])
  (result config prop labels tests-run status args args/smallest exception))

(module+ private
  (provide (struct-out result)))


;; check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 label!)

(define current-labels
  (make-parameter #f))

(define/contract (label! s)
  (-> (or/c false/c string?) void?)
  (define labels (current-labels))
  (when (and s labels)
    (hash-update! labels s add1 0)))

(define/contract (check c p)
  (-> config? prop? result?)
  (define caller-rng (current-pseudo-random-generator))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-labels (make-hash)]
                 [current-pseudo-random-generator rng])
    (match-define (config seed tests size deadline) c)
    (match-define (prop _name _arg-ids g f) p)

    (define exn? #f)
    (define (pass? args)
      (with-handlers ([(lambda (_) #t)
                       (lambda (the-exn)
                         (begin0 #f
                           (set! exn? the-exn)))])
        (parameterize ([current-pseudo-random-generator caller-rng])
          (apply f args))))

    (define (descend-shrinks trees last-failing-value)
      (cond
        [(stream-empty? trees) last-failing-value]
        [else
         (define tree (stream-first trees))
         (define value (shrink-tree-val tree))
         (if (pass? value)
             (descend-shrinks (stream-rest trees) last-failing-value)
             (descend-shrinks (shrink-tree-shrinks (stream-first trees)) value))]))

    (random-seed seed)
    (let loop ([test 0])
      (cond
        [(= test tests)
         (make-result c p (current-labels) test 'passed)]

        [(>= (current-inexact-milliseconds) deadline)
         (make-result c p (current-labels) (add1 test) 'timed-out)]

        [else
         (define tree (g rng (size (add1 test))))
         (define value (shrink-tree-val tree))
         (cond
           [(pass? value)
            (loop (add1 test))]

           [else
            (define shrunk?
              (parameterize ([current-labels #f])
                (descend-shrinks (shrink-tree-shrinks tree) #f)))
            (make-result c p (current-labels) (add1 test) 'falsified value shrunk? exn?)])]))))

(module+ private
  (provide check))

(module+ test
  (require (prefix-in ru: rackunit))

  (define-syntax-rule (check-status r s)
    (ru:check-equal? (result-status r) s))

  (check-status
   (check (make-config) prop-addition-commutes)
   'passed)

  (check-status
   (check (make-config) prop-addition-is-multiplication)
   'falsified))


;; common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-random-seed)
  (modulo
   (for/fold ([n 0])
             ([b (in-list (bytes->list (crypto-random-bytes 8)))])
     (arithmetic-shift (+ n b) 8))
   (expt 2 31)))
