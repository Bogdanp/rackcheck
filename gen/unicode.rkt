#lang racket/base

(require "base.rkt"
         "syntax.rkt")

(provide
 gen:unicode
 gen:unicode-letter
 gen:unicode-mark
 gen:unicode-number
 gen:unicode-punctuation
 gen:unicode-symbol
 gen:unicode-separator)

(define chars-mu (make-semaphore 1))
(define chars
  (make-hasheq))

(define (generate-chars)
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
     (if (memq category '(lu ll lt lm lo))
         (cons char letters)
         letters)
     (if (memq category '(mn mc me))
         (cons char marks)
         marks)
     (if (memq category '(nd nl no))
         (cons char numbers)
         numbers)
     (if (memq category '(pc pd ps pe pi pf po))
         (cons char puncts)
         puncts)
     (if (memq category '(sm sc sk so))
         (cons char symbols)
         symbols)
     (if (memq category '(zs zl zp))
         (cons char separators)
         separators))))

(define (try-load-chars!)
  (when (hash-empty? chars)
    (call-with-semaphore chars-mu
      (lambda ()
        (when (hash-empty? chars)
          (define-values (letters marks numbers puncts symbols separators)
            (generate-chars))
          (hash-set*!
           chars
           'letters letters
           'marks marks
           'numbers numbers
           'puncts puncts
           'symbols symbols
           'separators separators))))))

(define (get-chars category)
  (try-load-chars!)
  (hash-ref chars category))

(define gen:unicode
  (gen:char-in 0 #x10FFFF))

(define gen:unicode-letter
  (gen:delay (gen:one-of (get-chars 'letters))))

(define gen:unicode-mark
  (gen:delay (gen:one-of (get-chars 'marks))))

(define gen:unicode-number
  (gen:delay (gen:one-of (get-chars 'numbers))))

(define gen:unicode-punctuation
  (gen:delay (gen:one-of (get-chars 'puncts))))

(define gen:unicode-symbol
  (gen:delay (gen:one-of (get-chars 'symbols))))

(define gen:unicode-separator
  (gen:delay (gen:one-of (get-chars 'separators))))
