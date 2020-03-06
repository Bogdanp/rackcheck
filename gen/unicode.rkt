#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         "base.rkt")

(provide
 gen:unicode
 gen:unicode-letter
 gen:unicode-mark
 gen:unicode-number
 gen:unicode-punctuation
 gen:unicode-symbol
 gen:unicode-separator)

(begin-for-syntax
  (define-values (letters marks numbers puncts symbols separators)
    (for/fold ([letters    null]
               [marks      null]
               [numbers    null]
               [puncts     null]
               [symbols    null]
               [separators null])
              ([n (in-range 0 #x10FFFF)]
               #:when (or (< n #xD800)
                          (> n #xDFFF)))
      (define char (integer->char n))
      (define category (char-general-category char))
      (values
       (if (member category '(lu ll lt lm lo))
           (cons char letters)
           letters)
       (if (member category '(mn mc me))
           (cons char marks)
           marks)
       (if (member category '(nd nl no))
           (cons char numbers)
           numbers)
       (if (member category '(pc pd ps pe pi pf po))
           (cons char puncts)
           puncts)
       (if (member category '(sm sc sk so))
           (cons char symbols)
           symbols)
       (if (member category '(zs zl zp))
           (cons char separators)
           separators)))))

(define-syntax (define-codepoint-list stx)
  (syntax-parse stx
    [(_ which:id)
     (with-syntax ([id (format-id #'which "char-~a-list" #'which)]
                   [list-id (format-id #'which "~as" #'which)])
       #'(define-syntax (id stx)
           (datum->syntax stx (cons 'list list-id))))]))

(define-codepoint-list letter)
(define-codepoint-list mark)
(define-codepoint-list number)
(define-codepoint-list punct)
(define-codepoint-list symbol)
(define-codepoint-list separator)

(define gen:unicode
  (gen:char-in 0 #x10FFFF))

(define gen:unicode-letter
  (gen:one-of (char-letter-list)))

(define gen:unicode-mark
  (gen:one-of (char-mark-list)))

(define gen:unicode-number
  (gen:one-of (char-number-list)))

(define gen:unicode-punctuation
  (gen:one-of (char-punct-list)))

(define gen:unicode-symbol
  (gen:one-of (char-symbol-list)))

(define gen:unicode-separator
  (gen:one-of (char-separator-list)))
