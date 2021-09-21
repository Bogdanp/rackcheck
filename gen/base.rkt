#lang racket/base

(require racket/contract
         racket/list
         racket/random
         racket/stream
         "core.rkt"
         (submod "core.rkt" private)
         (submod "shrink-tree.rkt" private))

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

(define gen:natural
  (gen
   (lambda (rng size)
     (define n (random 0 (add1 size) rng))
     (make-shrink-tree n shrink-integer))))

(module+ test
  (require rackunit)

  (define-syntax-rule (tc name body ...)
    (test-case name
      (random-seed 1337)
      body ...))

  (define-check (check-samples/num n g expected)
    (check-equal? (sample g n) expected))
  (define-check (check-samples g expected)
    (check-samples/num 5 g expected))

  (define-check (check-shrinks*/sized size g expected)
    (check-equal? (shrink g size #:max-depth #f) expected))
  (define-check (check-shrinks* g expected)
    (check-shrinks*/sized 5 g expected))
  (define-check (check-shrinks/sized size g expected)
    (check-equal? (shrink g size) expected))
  (define-check (check-shrinks g expected)
    (check-shrinks/sized 5 g expected))

  (tc "naturals"
    (check-samples gen:natural '(0 0 4 5 8))
    (check-samples gen:natural '(0 1 3 6 4)))

  (tc "shrinking naturals"
    (check-shrinks           gen:natural '(1 (0)))
    (check-shrinks/sized  30 gen:natural '(5 (0) (3 ...) (4 ...)))
    (check-shrinks/sized 500 gen:natural '(468 (0) (234 ...) (351 ...) (410 ...) (439 ...) (454 ...) (461 ...) (465 ...) (467 ...)))
    (check-shrinks/sized 500 gen:natural '(297 (0) (149 ...) (223 ...) (260 ...) (279 ...) (288 ...) (293 ...) (295 ...) (296 ...)))))

(define/contract (gen:integer-in lo hi)
  (->i ([lo exact-integer?]
        [hi (lo) (>=/c lo)])
       [result gen?])
  (gen:map
   (gen:resize gen:natural (- hi lo))
   (λ (nat) (+ lo nat))))

(module+ test
  (tc "integer-in"
    (check-samples (gen:integer-in 0 20) '(6 3 19 12 10))
    (check-samples (gen:integer-in -5 5) '(3 1 3 2 -3)))

  (tc "shrinking integer-in"
    (check-shrinks          (gen:integer-in 0 20)    '(6 (0) (3 ...) (5 ...)))
    (check-shrinks/sized 30 (gen:integer-in -200 20) '(-161 (-200) (-180 ...) (-170 ...) (-165 ...) (-163 ...) (-162 ...)))))

(define gen:real
  (gen
   (lambda (rng _size)
     (make-shrink-tree (random rng)))))

(module+ test
  (tc "real"
    (check-samples gen:real '(0.2904158091187683 0.17902984405826025 0.9348212358175817 0.592848361775386 0.4846099332903666)))

  (tc "shrinking real"
    (check-shrinks gen:real '(0.2904158091187683))))

(define/contract (gen:one-of choices [equal?-proc equal?])
  (->* ((non-empty-listof any/c))
       ((-> any/c any/c boolean?))
       gen?)
  (gen
   (lambda (rng _size)
     (shrink-tree-map
      (make-shrink-tree
       (cons (random-ref choices rng) choices)
       (λ (choice&choices)
         (define prev-choice (car choice&choices))
         (define prev-choices (cdr choice&choices))
         (define remaining-choices (remove prev-choice prev-choices equal?-proc))
         (if (null? remaining-choices)
             (stream)
             (stream (cons (random-ref remaining-choices rng) remaining-choices)))))
      (λ (choice&choices)
        (car choice&choices))))))

(module+ test
  (tc "one-of"
    (check-samples (gen:one-of '(1 2 3)) '(3 1 2 1 3)))

  (tc "shrinking one-of"
    (check-shrinks  (gen:one-of '(1))     '(1))
    (check-shrinks  (gen:one-of '(1 2 3)) '(3 (1 ...)))
    (check-shrinks* (gen:one-of '(1 2 3)) '(1 (2 (3))))))

(define gen:boolean
  (gen
   (lambda (rng _size)
     (if (zero? (random 0 2 rng))
         (make-shrink-tree #f)
         (make-shrink-tree #t (λ (true?) (if true? (stream #f) empty-stream)))))))

(module+ test
  (tc "boolean"
    (check-samples gen:boolean '(#f #f #t #t #f)))

  (tc "shrinking boolean"
    (check-shrinks gen:boolean '(#f))
    (check-shrinks gen:boolean '(#f))
    (check-shrinks gen:boolean '(#t (#f)))))

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
  (-> gen? ... gen?)
  (gen
   (lambda (rng size)
     (define trees
       (for/list ([g (in-list gs)])
         (g rng size)))
     (shrink-tree-map
      (make-shrink-tree trees shrink-trees)
      (λ (ts) (map shrink-tree-val ts))))))

(module+ test
  (tc "tuple"
    (check-samples
     (gen:tuple gen:natural gen:char-digit)
     '((0 #\1) (1 #\5) (2 #\7) (6 #\7) (11 #\2))))

  (tc "shrinking tuple"
    (check-shrinks
     (gen:tuple gen:natural (gen:symbol gen:char-digit))
     '((1 |9|)
       ((0 |9|) ...)
       ((1 ||)  ...)
       ((1 |0|) ...)
       ((1 |5|) ...)
       ((1 |7|) ...)
       ((1 |8|) ...)))))

(define/contract (gen:list g #:max-length [max-len 128])
  (->* (gen?) (#:max-length exact-nonnegative-integer?) gen?)
  (gen
   (lambda (rng size)
     (define len (min (random 0 (add1 size) rng) max-len))
     (define trees
       (for/list ([_ (in-range len)])
         (g rng size)))
     (shrink-tree-map
      (make-shrink-tree trees shrink-list)
      (λ (ts) (map shrink-tree-val ts))))))

(module+ test
  (tc "list"
    (check-samples
     (gen:list gen:natural)
     '(()
       ()
       (2 2 3 3)
       (6 2 6 5 2 2 9)
       (2 13))))

  (tc "shrinking list"
    (check-equal?
     (shrink (gen:list gen:natural) 20 #:limit 11)
     '((3 19 12 10 16 12)
       (())
       ((10 16 12) ...)
       ((3 19 12) ...)
       ((19 12 10 16 12) ...)
       ((3 12 10 16 12) ...)
       ((3 19 10 16 12) ...)
       ((3 19 12 16 12) ...)
       ((3 19 12 10 12) ...)
       ((3 19 12 10 16) ...)
       ((0 19 12 10 16 12) ...)
       ((2 19 12 10 16 12) ...)
       ...))))

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

    (define-values (keys gens)
      (for/fold ([keys '()]
                 [gens '()])
                ([(v i) (in-indexed pairs)])
        (if (even? i)
            (values (cons v keys) gens)
            (values keys (cons v gens)))))

    (gen:map (apply gen:tuple gens)
             (lambda (tuple)
               (constructor (map cons keys tuple))))))

(define-syntax-rule (define-gen:hash id f)
  (define id (make-gen:hash 'id f)))

(define-gen:hash gen:hash make-immutable-hash)
(define-gen:hash gen:hasheq make-immutable-hasheq)
(define-gen:hash gen:hasheqv make-immutable-hasheqv)

(define/contract (gen:frequency freqs)
  (->i ([freqs (non-empty-listof (cons/c exact-nonnegative-integer? gen?))])
       #:pre (freqs) (ormap (λ (f) (> (car f) 0)) freqs)
       [result gen?])
  (define total
    (apply + (map car freqs)))
  (gen:bind
   (gen:no-shrink
    (gen:integer-in 0 (sub1 total)))
   (lambda (n)
     (let loop ([sum 0]
                [freqs freqs])
       (define pair (car freqs))
       (define next-sum (+ (car pair) sum))
       (if (> next-sum n)
           (cdr pair)
           (loop next-sum (cdr freqs)))))))

(module+ test
  (tc "frequency"
    (check-samples/num 10
      (gen:frequency
       `((7 . ,gen:natural)
         (5 . ,gen:char-letter)
         (0 . ,(gen:const 'not-this-one))
         (2 . ,(gen:string gen:char-letter #:max-length 8))))
      '(0 #\j "MK" "VccD" 7 18 #\G "FXYcZQxB" 19 #\u))))


;; shrinking helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (halves n)
  (if (zero? n)
      empty-stream
      (stream-cons n (halves (quotient n 2)))))

(define (shrink-integer n)
  (if (zero? n)
      empty-stream
      (stream-append
       (if (negative? n)
           (stream (abs n))
           empty-stream)
       (for/stream ([v (in-stream (halves n))])
         (- n v)))))

(define (shrink-list trees)
  (define (removes k n xs)
    (cond
      [(> k n) empty-stream]
      [(= k n) (stream null)]
      [else
       (define-values (xs-l xs-r)
         (split-at xs k))
       (stream-cons
        xs-r
        (for/stream ([r-xs (in-stream (removes k (- n k) xs-r))])
          (append xs-l r-xs)))]))
  (define len
    (length trees))
  (if (zero? len)
      empty-stream
      (stream-append
       (for*/stream ([k (in-stream (halves len))]
                     [l (in-stream (removes k len trees))])
         l)
       (shrink-trees trees))))

;; Local Variables:
;; eval: (put 'check-samples/num 'racket-indent-function #'defun)
;; eval: (put 'tc 'racket-indent-function #'defun)
;; End:
