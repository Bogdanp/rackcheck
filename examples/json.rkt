#lang racket/base

(module+ test
  (require json
           rackcheck
           rackcheck/gen/unicode)

  (define-syntax-rule (gen:recursive g)
    (make-gen
     (lambda (rng size)
       (g rng size))))

  (define gen:jsexpr
    (gen:frequency
     `((10 . ,gen:boolean)
       (10 . ,gen:natural)
       (10 . ,(gen:string gen:unicode #:max-length 20))
       (2  . ,(gen:recursive (gen:list gen:jsexpr #:max-length 5)))
       (2  . ,(gen:recursive (gen:let ([keys&vals (gen:list (gen:tuple (gen:symbol gen:unicode) gen:jsexpr) #:max-length 5)])
                               (make-immutable-hasheq keys&vals)))))))

  (check-property
   (property ([v gen:jsexpr])
     (equal? ((compose1 string->jsexpr jsexpr->string) v) v))))
