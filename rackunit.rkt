#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/string
         (only-in rackunit
                  current-check-handler
                  define-check
                  fail-check
                  make-check-info
                  make-check-location
                  with-check-info*)
         "prop.rkt"
         (submod "prop.rkt" private))

(provide
 (rename-out [check-property* check-property]))

(define-check (check-property p c)
  (define res (check c p))
  (case (result-status res)
    [(falsified)
     (define (format-args args)
       (string-join
        (for/list ([arg-id (in-list (prop-arg-ids (result-prop res)))]
                   [arg (in-list args)])
          (format "  ~a = ~s" arg-id arg))
        "\n"))

     (define message
       (if (result-args/smallest res)
           (format "Failed after ~a tests:\n\n~a\n\nShrunk:\n\n~a"
                   (result-tests-run res)
                   (format-args (result-args res))
                   (format-args (result-args/smallest res)))
           (format "Failed after ~a tests:\n\n~a\n\nCould not shrink."
                   (result-tests-run res)
                   (format-args (result-args res)))))

     (fail-check message)]

    [(timed-out)
     (fail-check (format "Timed out."))]))

(define-syntax (check-property* stx)
  (syntax-parse stx
    [(_ (~optional c:expr) p:expr)
     #:with location (datum->syntax stx (list 'list
                                              (syntax-source stx)
                                              (syntax-line stx)
                                              (syntax-column stx)
                                              (syntax-position stx)
                                              (syntax-span stx)))
     #'(let ([conf (~? c (make-config))])
         (with-check-info*
           (list
            (make-check-location location)
            (make-check-info 'name (prop-name p))
            (make-check-info 'seed (config-seed conf)))
           (lambda ()
             (check-property p conf))))]))
