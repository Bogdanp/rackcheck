#lang racket/base

(module private racket/base
  (require racket/stream)

  (provide (all-defined-out))

  (struct shrink-tree (val shrinks))

  (define in-shrink-tree-shrinks
    (compose1 in-stream shrink-tree-shrinks))

  (define (in-shrink-tree tree)
    (in-stream
     (stream-cons
      (shrink-tree-val tree)
      (let loop ([shrinks (shrink-tree-shrinks tree)])
        (cond
          [(stream-empty? shrinks) empty-stream]
          [else
           (define fst (stream-first shrinks))
           (define rst (stream-rest shrinks))
           (stream-cons
            (shrink-tree-val fst)
            (loop (stream-append rst (shrink-tree-shrinks fst))))])))))

  (define (shrink-tree->list tree)
    (for/list ([v (in-shrink-tree tree)])
      v))

  (define (make-shrink-tree init-v [proc (λ (_) empty-stream)])
    (shrink-tree
     init-v
     (for/stream ([v (proc init-v)])
       (make-shrink-tree v proc))))

  (define (shrink-tree-map tree proc)
    (shrink-tree
     (proc (shrink-tree-val tree))
     (for/stream ([v (in-shrink-tree-shrinks tree)])
       (shrink-tree-map v proc))))

  (define (shrink-tree-filter tree proc)
    (and (proc (shrink-tree-val tree))
         (shrink-tree
          (shrink-tree-val tree)
          (for*/stream ([subtree (in-shrink-tree-shrinks tree)]
                        [subtree (in-value (shrink-tree-filter subtree proc))]
                        #:when subtree)
            subtree))))

  (define (shrink-tree-join tree)
    (define inner-tree (shrink-tree-val tree))
    (unless (shrink-tree? inner-tree)
      (raise-argument-error 'shrink-tree-join "(shrink-tree/c shrink-tree?)" tree))
    (define inner-val (shrink-tree-val inner-tree))
    (define inner-shrinks (shrink-tree-shrinks inner-tree))
    (define outer-shrinks (shrink-tree-shrinks tree))
    (shrink-tree
     inner-val
     (stream-append
      (stream-map shrink-tree-join outer-shrinks)
      inner-shrinks)))

  (define (shrink-trees trees)
    (cond
      [(null? trees)
       empty-stream]

      [else
       (define fst (car trees))
       (define rst (cdr trees))
       (stream-append
        (for/stream ([shrunk-fst (in-stream (shrink-tree-shrinks fst))])
          (cons shrunk-fst rst))
        (for/stream ([shrunk-rsts (in-stream (shrink-trees rst))])
          (cons fst shrunk-rsts)))])))

(require racket/contract
         racket/stream
         'private)

(provide
 shrink-proc/c
 (contract-out
  (struct shrink-tree ([val any/c] [shrinks stream?]))
  [make-shrink-tree (->* (any/c) (shrink-proc/c) shrink-tree?)]
  [shrink-tree-map (-> shrink-tree? (-> any/c any/c) shrink-tree?)]))

(define shrink-proc/c
  (-> any/c stream?))

(module+ test
  (require rackunit)

  (define (make-int-tree start)
    (make-shrink-tree start (λ (v) (for/stream ([n (in-range (sub1 v) 0 -1)]) n))))

  (define t (make-int-tree 5))

  (check-equal?
   (shrink-tree->list t)
   '(5 4 3 2 1 3 2 1 2 1 1 2 1 1 1 1))

  (check-equal?
   (shrink-tree->list (shrink-tree-map t (λ (x) (+ x x))))
   '(10 8 6 4 2 6 4 2 4 2 2 4 2 2 2 2))

  (check-false
   (shrink-tree-filter t even?))

  (check-equal?
   (shrink-tree->list (shrink-tree-filter (stream-first (shrink-tree-shrinks t)) even?))
   '(4 2))

  (let ([tests `([() . ()]
                 [(,(make-int-tree 1)) . ()]
                 [(,(make-int-tree 3)) . ((2) (1))]
                 [(,(make-int-tree 3) ,(make-int-tree 3)) . ((2 3) (1 3) (3 2) (3 1))]
                 [(,(make-int-tree 1) ,(make-int-tree 5)) . ((1 4) (1 3) (1 2) (1 1))])])
    (for ([test (in-list tests)])
      (check-equal?
       (for*/list ([trees (in-stream (shrink-trees (car test)))])
         (map shrink-tree-val trees))
       (cdr test)))))
