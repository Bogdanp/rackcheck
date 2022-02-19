#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "core.rkt")

(provide
 gen:let
 gen:delay)

(define (gen:let-helper r)
  (cond
    [(gen? r) r]
    [else (gen:const r)]))

(define-syntax (gen:let stx)
  (syntax-parse stx
    [(_ () body ...+)
     #'(gen:let-helper (let () body ...))]

    [(_ ([id:id g:expr] . r) body ...+)
     #'(gen:bind g (lambda (id)
                     (gen:let r body ...)))]))

(define-syntax-rule (gen:delay g)
  (make-gen
   (lambda (rng size)
     (g rng size))))

(module+ test
  (require rackunit
           "base.rkt")

  (define (reseed)
    (random-seed 1337))

  (define g
    (gen:let ([a gen:natural]
              [b gen:natural])
      (* a b)))

  (reseed)
  (check-equal?
   (sample g 5)
   '(0 0 8 36 14))

  (define gen:variable
    (gen:let ([h gen:char-letter]
              [t (gen:string (gen:choice gen:char-letter gen:char-digit))])
      (string->symbol
       (apply string h (string->list t)))))

  (reseed)
  (check-equal?
   (sample gen:variable 3)
   '(e G4 u5)))

;; Local Variables:
;; eval: (put 'gen:let 'racket-indent-function #'defun)
;; End:
