#lang racket/base

(require racket/contract
         racket/function
         racket/list
         racket/match
         racket/promise
         racket/random
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
 ;gen:hash
 ;gen:hasheq
 ;gen:hasheqv
 gen:frequency)

(define (halves n)
  (let loop ([n (quotient n 2)])
    (if (zero? n)
        '()
        (cons n (loop (quotient n 2))))))

(define (shrink-integer n)
  (if (zero? n)
      '()
      (append (if (negative? n) (list (abs n)) '())
              (cons 0 (map (curry - n) (halves n))))))

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
    (check-values (9 '((0) (5 3 0) (7 4 0) (8 6 5 3 2 1 0)))
      (sample-shrink gen:natural))

    (check-values (396 '((393 387 291 273 271 267 263 0)
                         (198 149 148 130 98 49 25 13 ...)
                         (372 361 350 348 347 337 327 322 ...)
                         (384 0)))
      (sample-shrink gen:natural 500))

    (check-values (74 '((0)
                        (73 69 52 51 39 35 33 29 ...)
                        (56 28 0)
                        (65 64 56 0)))
      (sample-shrink gen:natural 300))))

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
    (check-values (6 '((0) (3 2 0) (5 0)))
      (sample-shrink (gen:integer-in 0 20)))

    (check-values (-28 '((-200)
                         (-114 -116 -117 -137 -140 -155 -156 -167 ...)
                         (-30 -32 -33 -43 -200)
                         (-38 -78 -93 -146 -149 -150 -175 -187 ...)))
      (sample-shrink (gen:integer-in -200 20)))))

(define gen:real
  (gen
   (lambda (rng _size)
     (make-shrink-tree (random rng)))))

(define/contract (gen:one-of xs)
  (-> (non-empty-listof any/c) gen?)
  (gen
   (lambda (rng _size)
     (make-shrink-tree (random-ref xs rng)))))

(define gen:boolean
  (gen
   (lambda (rng _size)
     (case (random 0 2 rng)
       [(0) (make-shrink-tree #f)]
       [(1) (make-shrink-tree #t (lazy (list (make-shrink-tree #f))))]))))

(module+ test
  (tc "boolean"
    (check-equal? (sample gen:boolean 5)
                  '(#f #f #t #t #f)))

  (tc "shrinking boolean"
    (check-values (#f '())
      (sample-shrink gen:boolean))

    (check-values (#f '())
      (sample-shrink gen:boolean))

    (check-values (#t '((#f)))
      (sample-shrink gen:boolean))))

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
     (let ([xs (map (lambda (g) (g rng size)) gs)])
       (shrink-tree-map
        (curry map value)
        (build-shrink-tree xs shrink-one))))))

(module+ test
  (tc "tuple"
    (check-equal? (sample (gen:tuple gen:natural gen:char-digit) 4)
                  '((0 #\1) (1 #\5) (2 #\7) (6 #\7))))

  (tc "shrinking tuple"
    (check-values ('(6 |954|)
                   '(((6 |904|)
                      (6 |900|)
                      (5 |900|)
                      (4 |900|)
                      (4 |800|)
                      (4 |00|)
                      (0 |00|)
                      (0 |0|)
                      ...)
                     ((6 |95|)
                      (6 |93|)
                      (6 ||)
                      (3 ||)
                      (0 ||))))
      (sample-shrink (gen:tuple gen:natural (gen:symbol gen:char-digit)) 20 2 8))))

(define (shrink-one xs)
  (match xs
    ['() '()]
    [(cons x xs)
     (append (for/list ([shrunk-x (shrink x)])
               (cons shrunk-x xs))
             (for/list ([shrunk-xs (shrink-one xs)])
               (cons x shrunk-xs)))]))

(define (removes k n xs)
  (cond
    [(> k n) '()]
    [(= k n) '(())]
    [else (let-values ([(xs-l xs-r) (split-at xs k)])
            (cons xs-r (for/list ([r-xs (removes k (- n k) xs-r)])
                         (append xs-l r-xs))))]))

(define (shrink-list xs)
  (let ([n (length xs)])
    (if (= n 0)
        '()
        (append
         (append* (for/list ([k (cons n (halves n))])
                    (removes k n xs)))
         (shrink-one xs)))))

(define/contract (gen:list g #:max-length [max-len 128])
  (->* (gen?) (#:max-length exact-nonnegative-integer?) gen?)
  (gen
   (lambda (rng size)
     (let* ([len (min (random 0 (add1 size) rng) max-len)]
            [xs (for/list ([_ (in-range len)])
                  (g rng size))])
       (shrink-tree-map
        (curry map value)
        (build-shrink-tree xs shrink-list))))))

(module+ test
  (tc "list"
    (check-equal? (sample (gen:list gen:natural) 4)
                  '(()
                    ()
                    (2 2 3 3)
                    (6 2 6 5 2 2 9))))

  (tc "shrinking list"
    (check-values ('(3 19 12 10 16 12)
                   '(((3 19 12 10 12 12)
                      (3 19 12 10 6 12)
                      (3 19 12 8 6 12)
                      (8 6 12)
                      (8 6 6)
                      (8 6 3)
                      (8 6 0)
                      (8 3 0)
                      ...)
                     ((3 12 10 16 12)
                      (3 0 10 16 12)
                      (0 0 10 16 12)
                      (0 0 10 16)
                      (0 0 10)
                      (0 10)
                      ())))
      (sample-shrink (gen:list gen:natural) 20 2 8))))

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

#;(define (make-gen:hash who constructor)
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

#;(define-syntax-rule (define-gen:hash id f)
  (define id (make-gen:hash 'id f)))

;(define-gen:hash gen:hash make-immutable-hash)
;(define-gen:hash gen:hasheq make-immutable-hasheq)
;(define-gen:hash gen:hasheqv make-immutable-hasheqv)

(define/contract (gen:frequency freqs)
  (-> (non-empty-listof (cons/c exact-positive-integer? gen?)) gen?)
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
                              (2 . ,(gen:string gen:char-letter))))
             10)
     '(0 1 "uU" #\u 13 #\U #\R #\G #\q 51))))

;; Local Variables:
;; eval: (put 'check-values 'racket-indent-function #'defun)
;; eval: (put 'tc 'racket-indent-function #'defun)
;; End:
