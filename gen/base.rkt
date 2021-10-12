#lang racket/base

(require racket/contract
         racket/function
         racket/list
         racket/match
         racket/promise
         racket/random
         racket/stream
         "core.rkt"
         (submod "core.rkt" private))

(provide
 (all-from-out "core.rkt")

 halves
 shrink-integer
 shrink-one
 removes
 list-cuts
 shrink-list

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
 gen:tuple*
 gen:list
 gen:list-len
 gen:vector
 gen:bytes
 gen:string
 gen:symbol
 gen:hash
 gen:hasheq
 gen:hasheqv
 gen:frequency)

(define/contract (halves n)
  (-> exact-integer? (stream/c exact-integer?))
  (if (zero? n)
      empty-stream
      (stream-cons n (halves (quotient n 2)))))

(define/contract (shrink-integer n)
  (-> exact-integer? (stream/c exact-integer?))
  (if (zero? n)
      empty-stream
      (stream-append (if (negative? n)
                         (stream (abs n)) empty-stream)
                     (stream-map (curry - n) (halves n)))))

(define gen:natural
  (gen
   (lambda (rng size)
     (let ([n (random 0 (add1 size) rng)])
       (build-shrink-tree n shrink-integer)))))

(define gen:integer
  (gen
   (lambda (rng size)
     (let ([n (random (- size) (add1 size) rng)])
       (build-shrink-tree n shrink-integer)))))

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
    (check-equal? (full-shrink gen:natural)
                  '(9 (0) (5 ...) (7 ...) (8 ...)))

    (check-equal? (full-shrink gen:natural 500)
                  '(89 (0) (45 ...) (67 ...) (78 ...) (84 ...) (87 ...) (88 ...)))

    (check-equal? (full-shrink gen:natural 300)
                  '(281 (0) (141 ...) (211 ...) (246 ...) (264 ...) (273 ...)
                        (277 ...) (279 ...) (280 ...)))))

(define/contract (gen:integer-in lo hi)
  (->i ([lo exact-integer?]
        [hi (lo) (>=/c lo)])
       [result gen?])
  (gen:map (gen:resize gen:natural (- hi lo))
           (curry + lo)))

(module+ test
  (tc "integer-in"
    (check-equal? (sample (gen:integer-in 0 20) 5) '(6 3 19 12 10))
    (check-equal? (sample (gen:integer-in -5 5) 5) '(3 1 3 2 -3)))

  (tc "shrinking integer-in"
      
    (check-equal? (full-shrink (gen:integer-in 0 20))
                    '(6 (0) (3 ...) (5 ...)))

    (check-equal? (full-shrink (gen:integer-in -200 20))
                  '(-161 (-200) (-180 ...) (-170 ...) (-165 ...)
                         (-163 ...) (-162 ...)))))

(define gen:real
  (gen
   (lambda (rng _size)
     (shrink-tree (random rng) empty-stream))))

(define/contract (gen:one-of xs)
  (-> (non-empty-listof any/c) gen?)
  (gen
   (lambda (rng _size)
     (shrink-tree (random-ref xs rng) empty-stream))))

(define gen:boolean
  (gen
   (lambda (rng _size)
     (case (random 0 2 rng)
       [(0) (shrink-tree #f empty-stream)]
       [(1) (shrink-tree #t (stream (shrink-tree #f empty-stream)))]))))

(module+ test
  (tc "boolean"
    (check-equal? (sample gen:boolean 5)
                  '(#f #f #t #t #f)))

  (tc "shrinking boolean"
      (check-equal? (full-shrink gen:boolean) '(#f))
      
      (check-equal? (full-shrink gen:boolean) '(#f))
      
      (check-equal? (full-shrink gen:boolean) '(#t (#f)))))

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
     (let ([xs (map (lambda (g) (g rng size)) gs)])
       (shrink-tree-map
        (curry map value)
        (build-shrink-tree xs (curry shrink-one shrink)))))))

(define/contract (gen:tuple* gs)
  (-> (listof gen?) gen?)
  (apply gen:tuple gs))

(module+ test
  (tc "tuple"
    (check-equal? (sample (gen:tuple gen:natural gen:char-digit) 4)
                  '((0 #\1) (1 #\5) (2 #\7) (6 #\7))))

  (tc "shrinking tuple"
      (check-equal? (full-shrink (gen:tuple gen:natural (gen:symbol gen:char-digit))
                                 20 #:first-n 6)
                    '((6 |954|)
                      ((0 |954|) ...)
                      ((3 |954|) ...)
                      ((5 |954|) ...)
                      ((6 ||) ...)
                      ((6 |54|) ...)
                      ((6 |94|) ...)
                      ...))))

(define/contract (shrink-one shr xs)
  (-> (-> any/c (stream/c any/c)) (listof any/c)
      (stream/c (listof any/c)))
  (match xs
    ['() empty-stream]
    [(cons x xs)
     (stream-append
      (for/stream ([shrunk-x (shr x)])
        (cons shrunk-x xs))
      (for/stream ([shrunk-xs (shrink-one shr xs)])
        (cons x shrunk-xs)))]))

(define/contract (removes k n xs)
  (-> exact-nonnegative-integer? exact-nonnegative-integer? (listof any/c)
      (stream/c (listof any/c)))
  (cond
    [(> k n) empty-stream]
    [(= k n) (stream '())]
    [else (let-values ([(xs-l xs-r) (split-at xs k)])
            (stream-cons xs-r (for/stream ([r-xs (removes k (- n k) xs-r)])
                                (append xs-l r-xs))))]))

(define (stream-flatten stream-streams)
  (if (stream-empty? stream-streams)
      empty-stream
      (stream-append
       (stream-first stream-streams)
       (stream-flatten (stream-rest stream-streams)))))

(define/contract (list-cuts l)
  (-> (listof any/c) (stream/c (listof any/c)))
  (let ([n (length l)])
    (stream-flatten
     (for/stream ([k (halves n)])
       (removes k n l)))))

(define/contract (shrink-list shr xs)
  (-> (-> any/c (stream/c any/c)) (stream/c any/c)
      (stream/c (listof any/c)))
  (if (= (length xs) 0)
      empty-stream
      (stream-append
       (list-cuts xs)
       (shrink-one shr xs))))

(define/contract (gen:list g #:max-length [max-len 128])
  (->* (gen?) (#:max-length exact-nonnegative-integer?) gen?)
  (gen
   (lambda (rng size)
     (let* ([len (min (random 0 (add1 size) rng) max-len)]
            [xs (for/list ([_ (in-range len)])
                  (g rng size))])
       (shrink-tree-map
        (curry map value)
        (build-shrink-tree xs (curry shrink-list shrink)))))))

(define/contract (gen:list-len g len)
  (-> gen? exact-nonnegative-integer? gen?)
  (gen
   (lambda (rng size)
     (let ([xs (for/list ([_ (in-range len)])
                 (g rng size))])
       (shrink-tree-map
        (curry map value)
        (build-shrink-tree xs (curry shrink-one shrink)))))))

(module+ test
  (tc "list"
    (check-equal? (sample (gen:list gen:natural) 4)
                  '(()
                    ()
                    (2 2 3 3)
                    (6 2 6 5 2 2 9))))

  (tc "shrinking list"
      (check-equal? (full-shrink (gen:list gen:natural) 20 #:first-n 11)
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

    (let-values ([(keys gens)
                  (for/fold ([keys '()]
                             [gens '()])
                            ([(v i) (in-indexed pairs)])
                    (if (even? i)
                        (values (cons v keys) gens)
                        (values keys (cons v gens))))])
      (gen:map (apply gen:tuple gens)
               (lambda (tuple)
                 (constructor (map cons keys tuple)))))))

(define-syntax-rule (define-gen:hash id f)
  (define id (make-gen:hash 'id f)))

(define-gen:hash gen:hash make-immutable-hash)
(define-gen:hash gen:hasheq make-immutable-hasheq)
(define-gen:hash gen:hasheqv make-immutable-hasheqv)

(define/contract (gen:frequency freqs)
  (->i ([freqs (non-empty-listof (cons/c exact-nonnegative-integer? gen?))])
       #:pre (freqs) (ormap (λ (f) (> (car f) 0)) freqs)
       [result gen?])
  (define total (apply + (map car freqs)))
  (let ([total (apply + (map car freqs))])
    (gen:bind
     (gen:no-shrink
      (gen:integer-in 0 (sub1 total)))
     (lambda (n)
       (let loop ([sum 0]
                  [freqs freqs])
         (let* ([pair (car freqs)]
                [sum* (+ (car pair) sum)])
           (if (> sum* n)
               (cdr pair)
               (loop sum* (cdr freqs)))))))))

(module+ test
  (tc "frequency"
    (check-equal?
     (sample (gen:frequency `((7 . ,gen:natural)
                              (5 . ,gen:char-letter)
                              (0 . ,(gen:const 'not-this-one))
                              (2 . ,(gen:string gen:char-letter #:max-length 8))))
             10)
     '(0 #\j "MK" "VccD" 7 18 #\G "FXYcZQxB" 19 #\u))))

;; Local Variables:
;; eval: (put 'check-values 'racket-indent-function #'defun)
;; eval: (put 'tc 'racket-indent-function #'defun)
;; End:
