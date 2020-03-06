#lang racket/base

(struct db (entries)
  #:transparent)

(define (make-db)
  (db null))

(define (db-set d k v)
  (db (cons (cons k v) (db-entries d))))

(define (db-remove d k)
  (db (for/list ([e (in-list (db-entries d))]
                 #:unless (eq? (car e) k))
        e)))

(define (db-clear _d)
  (db null))

(module+ test
  (require rackcheck
           racket/match)

  (define gen:key
    (gen:symbol gen:char-letter))

  (define gen:op
    (gen:let ([keys (gen:resize 10 (gen:non-empty (gen:list gen:key)))]
              [actions (gen:list
                        (gen:amb
                         (gen:tuple (gen:const 'set) (gen:one-of keys) gen:natural)
                         (gen:tuple (gen:const 'remove) (gen:one-of keys))
                         (gen:tuple (gen:const 'clear))))])
      (cons '(new) actions)))

  (define/match (step-db db op)
    [(_  (list 'new))      (make-db)]
    [(db (list 'set k v))  (db-set    db k v)]
    [(db (list 'remove k)) (db-remove db k)]
    [(db (list 'clear))    (db-clear  db)])

  (define/match (step-hash h op)
    [(_ (list 'new))      (hasheq)]
    [(h (list 'set k v))  (hash-set    h k v)]
    [(h (list 'remove k)) (hash-remove h k)]
    [(h (list 'clear))    (hash-clear  h)])

  (define (interpret how ops)
    (for/fold ([res #f])
              ([op (in-list ops)])
      (how res op)))

  (define-property prop-like-a-hash
    ([ops gen:op])
    (define db (interpret step-db   ops))
    (define h  (interpret step-hash ops))
    (equal? (sort (db-entries db) symbol<? #:key car)
            (sort (hash->list h)  symbol<? #:key car)))

  (check-property prop-like-a-hash))
