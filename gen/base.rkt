#lang racket/base

(require racket/contract
         racket/list
         racket/random
         racket/sequence
         racket/stream
         "../private/stream.rkt"
         "core.rkt"
         (submod "core.rkt" private))

(provide
 (all-from-out "core.rkt")

 gen:natural
 gen:integer-in
 gen:real
 gen:one-of
 gen:boolean
 gen:char
 gen:char-in
 gen:char-letter
 gen:char-digit
 gen:char-alphanumeric
 gen:tuple
 gen:list
 gen:vector
 gen:bytes
 gen:string
 gen:symbol
 gen:hash
 gen:hasheq
 gen:hasheqv
 gen:frequency)

(define (halves n)
  (let loop ([n (quotient n 2)])
    (if (zero? n)
        (stream 0)
        (stream* n (loop (quotient n 2))))))

(define k 4294967087)
(define gen:natural
  (gen
   (lambda (rng size)
     (define n (random (min k (add1 size)) rng))
     (stream-cons n (halves n)))))

(module+ test
  (require rackunit)

  (define-syntax-rule (tc name body ...)
    (test-case name
      (random-seed 1337)
      body ...))

  (define-syntax-rule (check-values (v ...) body ...)
    (call-with-values
     (lambda ()
       body ...)
     (lambda vs
       (check-equal? vs (list v ...)))))

  (tc "naturals"
    (check-equal? (sample gen:natural 5) '(0 0 4 5 8))
    (check-equal? (sample gen:natural 5) '(0 1 3 6 4)))

  (tc "shrinking naturals"
    (check-values (9 '(4 2 1 0))
      (shrink gen:natural))

    (check-values (89 '(44 22 11 5 2 1 0))
      (shrink gen:natural 500))

    (check-values (281 '(140 70 35 17 8 4 2 1 0))
      (shrink gen:natural 300))))

(define/contract (gen:integer-in lo hi)
  (->i ([lo exact-integer?]
        [hi (lo) (>=/c lo)])
       [result gen?])
  (gen
   (lambda (rng _size)
     (define n (random lo (add1 hi) rng))
     (stream* n (stream-filter
                 (lambda (x)
                   (>= x lo))
                 (halves n))))))

(module+ test
  (tc "integer-in"
    (check-equal? (sample (gen:integer-in 0 20) 5) '(6 3 19 12 10))
    (check-equal? (sample (gen:integer-in -5 5) 5) '(3 1 3 2 -3)))

  (tc "shrinking integer-in"
    (check-values (6 '(3 1 0))
      (shrink (gen:integer-in 0 20)))

    (check-values (-161 '(-80 -40 -20 -10 -5 -2 -1 0))
      (shrink (gen:integer-in -200 20)))))

(define gen:real
  (gen
   (lambda (rng _size)
     (stream (random rng)))))

(define/contract (gen:one-of xs)
  (-> (non-empty-listof any/c) gen?)
  (gen
   (lambda (rng _size)
     (stream (random-ref xs rng)))))

(define gen:boolean
  (gen
   (lambda (rng _size)
     (case (random 0 2 rng)
       [(0) (stream #f)]
       [(1) (stream #t #f)]))))

(module+ test
  (tc "boolean"
    (check-equal? (sample gen:boolean 5)
                  '(#f #f #t #t #f)))

  (tc "shrinking boolean"
    (check-values (#f '())
      (shrink gen:boolean))

    (check-values (#f '())
      (shrink gen:boolean))

    (check-values (#t '(#f))
      (shrink gen:boolean))))

(define char-integer/c
  (or/c (integer-in 0      #xD7FF)
        (integer-in #xE000 #x10FFFF)))

(define/contract (gen:char-in lo hi)
  (->i ([lo char-integer/c]
        [hi (lo) (and/c char-integer/c (>/c lo))])
       [result gen?])
  (define g
    (if (or (and (< lo #xD800)
                 (< hi #xD800))
            (and (> lo #xDFFF)
                 (> hi #xDFFF)))
        (gen:integer-in lo hi)
        (gen:choice
         (gen:integer-in lo #xD7FF)
         (gen:integer-in #xE000 hi))))

  (gen:map g integer->char))

(define gen:char
  (gen:char-in 0 255))

(define gen:char-letter
  (gen:choice
   (gen:char-in 65 90)
   (gen:char-in 97 122)))

(define gen:char-digit
  (gen:char-in 48 57))

(define gen:char-alphanumeric
  (gen:choice
   gen:char-letter
   gen:char-digit))

(define/contract (gen:tuple . gs)
  (-> gen? gen? ... gen?)
  (gen
   (lambda (rng size)
     (define seqs
       (for/list ([g (in-list gs)])
         (g rng size)))

     ;; TODO: shrink via product.
     (for/stream ([t (sequence-map
                      (lambda vals vals)
                      (apply in-parallel seqs))])
       t))))

(module+ test
  (tc "tuple"
    (check-equal? (sample (gen:tuple gen:natural gen:char-digit) 4)
                  '((0 #\1)
                    (1 #\5)
                    (2 #\7)
                    (6 #\7))))

  (tc "shrinking tuple"
    (check-values ('(9 |95476|)
                   '((4 |9547|)
                     (2 |954|)
                     (1 |95|)
                     (0 |9|)))
      (shrink (gen:tuple gen:natural (gen:symbol gen:char-digit))))))

(define/contract (gen:list g #:max-length [max-len 128])
  (->* (gen?) (#:max-length exact-nonnegative-integer?) gen?)
  (gen:bind
   (gen:no-shrink gen:natural)
   (lambda (len)
     (gen
      (lambda (rng size)
        (define seq
          (let ([len (min len max-len)])
            (for/list ([_ (in-range len)])
              (stream-first (g rng size)))))

        (stream*
         seq
         (for/stream ([n (in-range (length seq) -1 -1)])
           (take seq n))))))))

(module+ test
  (tc "list"
    (check-equal? (sample (gen:list gen:natural) 4)
                  '(()
                    ()
                    (2 2 3 3)
                    (6 2 6 5 2 2 9))))

  (tc "shrinking list"
    (check-values ('(5 28 18 15 24 18 24 20 7)
                   '((5 28 18 15 24 18 24 20)
                     (5 28 18 15 24 18 24)
                     (5 28 18 15 24 18)
                     (5 28 18 15 24)
                     (5 28 18 15)
                     (5 28 18)
                     (5 28)
                     (5)
                     ()))
      (shrink (gen:list gen:natural)))))

(define gen:vector
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (gen:map (keyword-apply gen:list kws kw-args args)
              list->vector))))

(define gen:bytes
  (make-keyword-procedure
   (lambda (kws kw-args [g (gen:integer-in 0 255)] . args)
     (gen:map (keyword-apply gen:list kws kw-args g args)
              list->bytes))))

(define gen:string
  (make-keyword-procedure
   (lambda (kws kw-args [g gen:char] . args)
     (gen:map (keyword-apply gen:list kws kw-args g args)
              list->string))))

(define gen:symbol
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (gen:map (keyword-apply gen:string kws kw-args args)
              string->symbol))))

(define (make-gen:hash who constructor)
  (lambda pairs
    (unless (even? (length pairs))
      (raise-argument-error who "an even number of arguments" pairs))

    (gen
     (lambda (rng size)
       (define-values (keys streams)
         (for/fold ([keys    null]
                    [streams null])
                   ([(v i) (in-indexed pairs)])
           (if (even? i)
               (values (cons v keys) streams)
               (values keys (cons (v rng size) streams)))))

       (define streams-for-keys
         (for/list ([k (in-list keys)]
                    [s (in-list streams)])
           (for/stream ([v (in-stream s)])
             (cons k v))))

       (stream-dedupe
        (for*/stream ([s (in-list streams-for-keys)]
                      [p (in-stream s)]
                      [h (sequence-map
                          (lambda vals
                            (constructor (append vals (list p))))
                          (apply in-parallel streams-for-keys))])
          h))))))

(define-syntax-rule (define-gen:hash id f)
  (define id (make-gen:hash 'id f)))

(define-gen:hash gen:hash make-immutable-hash)
(define-gen:hash gen:hasheq make-immutable-hasheq)
(define-gen:hash gen:hasheqv make-immutable-hasheqv)

(define/contract (gen:frequency freqs)
  (-> (non-empty-listof (cons/c exact-positive-integer? gen?)) gen?)
  (define total (apply + (map car freqs)))
  (gen:bind
   (gen:no-shrink
    (gen:integer-in 0 (sub1 total)))
   (lambda (x)
     (let loop ([sum 0]
                [freqs freqs])
       (define pair (car freqs))
       (define sum* (+ (car pair) sum))
       (if (> sum* x)
           (cdr pair)
           (loop sum* (cdr freqs)))))))

(module+ test
  (tc "frequency"
    (check-equal?
     (sample (gen:frequency `((7 . ,gen:natural)
                              (5 . ,gen:char-letter)
                              (2 . ,(gen:string gen:char-letter))))
             10)
     '(0 "u" #\R 6 #\g "uoXa" 8 #\e 10 5))))

;; Local Variables:
;; eval: (put 'check-values 'racket-indent-function #'defun)
;; eval: (put 'tc 'racket-indent-function #'defun)
;; End:
