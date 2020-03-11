#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/match
         racket/random
         racket/stream
         "gen/syntax.rkt")

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
    [(_ (~optional name:id)
        ([id:id g:expr] ...)
        body ...+)
     #'(prop (~? 'name 'unnamed)
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
        #:deadline exact-nonnegative-integer?)
       config?)
  (config seed tests size deadline))

(module+ private
  (provide (struct-out config)))


;; result ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct result (config prop tests-run status args args/smallest e)
  #:transparent)

(define (make-result config prop tests-run [status 'passed] [args #f] [args/smallest #f] [exception #f])
  (result config prop tests-run status args args/smallest exception))

(module+ private
  (provide (struct-out result)))


;; check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (check c p)
  (-> config? prop? result?)
  (define caller-rng (current-pseudo-random-generator))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (match-define (config seed tests size deadline) c)
    (match-define (prop name arg-ids g f) p)

    (define e #f)
    (define (pass? args)
      (with-handlers ([(lambda _ #t)
                       (lambda (the-exn)
                         (begin0 #f
                           (set! e the-exn)))])
        (parameterize ([current-pseudo-random-generator caller-rng])
          (apply f args))))

    (random-seed seed)
    (let loop ([test 1])
      (cond
        [(= test tests)
         (make-result c p test)]

        [(>= (current-inexact-milliseconds) deadline)
         (make-result c p test 'timed-out)]

        [else
         (define s (g rng (size test)))
         (define v (stream-first s))
         (cond
           [(pass? v)
            (loop (add1 test))]

           [else
            (define smallest
              (for/fold ([smallest #f])
                        ([v (in-stream (stream-rest s))])
                (cond
                  [(pass? v) smallest]
                  [else v])))

            (make-result c p test 'falsified v smallest e)])]))))

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
