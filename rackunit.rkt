#lang racket/base

(require (for-syntax racket/base
                     racket/format
                     syntax/parse)
         racket/port
         (except-in rackunit check)
         (only-in rackunit/private/check-info current-check-info)
         "prop.rkt"
         (submod "prop.rkt" private))

(provide
 (rename-out [check-property* check-property]))

(define-check (check-property p c)
  (define res (check c p))
  (case (result-status res)
    [(falsified)
     (define e (result-e res))

     (define (display-args args)
       (for ([arg-id (in-list (prop-arg-ids (result-prop res)))]
             [arg (in-list args)])
         (displayln (format "  ~a = ~s" arg-id arg))))

     (define message
       (with-output-to-string
         (lambda ()
           (displayln (format "Failed after ~a tests:" (result-tests-run res)))

           (newline)
           (display-args (result-args res))

           (newline)
           (cond
             [(result-args/smallest res)
              => (lambda (args)
                   (displayln "Shrunk:")
                   (newline)
                   (display-args args))]

             [else
              (displayln "Could not shrink.")])

           (when (and (result-e res) (not (exn:test:check? (result-e res))))
             (parameterize ([current-error-port (current-output-port)])
               (newline)
               (displayln "Exception:")
               ((error-display-handler)
                (exn-message (result-e res))
                (result-e res)))))))

     (parameterize ([current-check-info (if (exn:test:check? e)
                                            (exn:test:check-stack e)
                                            (current-check-info))])
       (fail-check message))]

    [(timed-out)
     (fail-check (format "Timed out after ~a tests." (result-tests-run res)))]))

(define-syntax (check-property* stx)
  (syntax-parse stx
    [(_ (~optional c:expr) p:expr)
     #:with location (datum->syntax stx (list 'list
                                              (let ([source (syntax-source stx)])
                                                (cond
                                                  [(path? source) source]
                                                  [else (~a source)]))
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
