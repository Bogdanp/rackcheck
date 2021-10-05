#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     racket/contract
                     racket/string
                     rackcheck
                     rackunit))

@(begin
  (define ev (make-base-eval))
  (ev '(require rackcheck racket/list racket/stream rackunit))
  (define-syntax-rule (ex body ...)
    (begin
     (random-seed 1337)
     (examples
      #:eval ev
      #:label #f
      body ...))))

@title{@tt{rackcheck}: property testing}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

Rackcheck is a property-based testing library for Racket with support
for shrinking.

I am still in the process of experimenting with the implementation of
this library so things may change without notice at this point.

@section{What about quickcheck?}

I initially started by forking the quickcheck library to add support
for shrinking, but found that I would have to make many breaking
changes to get shrinking to work the way I wanted so I decided to
start from scratch instead.

@section{Reference}
@defmodule[rackcheck]

@subsection{Generators}

Generators produce arbitrary values based upon certain constraints.
The generators provided by this library can be mixed and matched in
order to produce complex values suitable for any domain.

By convention, all generators and combinators are prefixed with @racket[gen:].

@subsubsection{Debugging}

The following functions come in handy when debugging generators.
Don't use them to produce values for your tests.

@defproc[(sample [g gen?]
                 [n exact-positive-integer?]
                 [rng pseudo-random-generator? (current-pseudo-random-generator)]) (listof any/c)]{

  Samples @racket[n] values from @racket[g].
}

@defproc[(full-shrink [g gen?]
                      [size exact-nonnegative-integer?]
                      [#:first-n first-n? (or/c false/c exact-nonnegative-integer?) #f]
                      [#:max-depth max-depth? (or/c false/c exact-nonnegative-integer?) #f]
                      [rng pseudo-random-generator? (current-pseudo-random-generator)])
         (listof any/c)]{

 Produces a value from @racket[g] and evaluates it's shrink tree.
 The evaluation searches the @racket[first-n?] shrinks of each term, up to a
 depth of @racket[max-depth?], or unbounded if @racket[#f].
}                                                                                                           

@subsubsection{Core Combinators}

@defthing[generator/c (-> pseudo-random-generator? exact-nonnegative-integer? shrink-tree?)]{
  The contract for generator functions.  Generator functions produce a
  lazily evaluated tree containing the different ways that the generated
  value can be shrunk.

  In general, you won't have to write generator functions yourself.
  Instead, you'll use the generators and combinators provided by this
  library to generate values for your domain.  That said, there may be
  cases where you want to tightly control how values are generated or
  shrunk and that's when you might reach for a custom generator.
}

@deftogether[(
  @defproc[(gen? [v any/c]) boolean?]
  @defproc[(make-gen [f generator/c]) gen?]
)]{
  @racket[gen?] returns @racket[#t] when @racket[v] is a generator
  value and @racket[make-gen] creates a new generator from a
  generator function.

  Use custom generators sparingly. They're one area where we might
  break backward compatibility if we come up with a better
  implementation.
}

@deftogether[(
  @defproc[(shrink-tree? [v any/c]) boolean?]
  @defproc[(make-shrink-tree [v any/c] [shrs (promise/c (listof shrink-tree?))]) shrink-tree?]
)]{

 A shrink-tree is a lazyily evaluated tree containing a value and the ways it
 can be shrunk. @racket[shrink-tree?] returns @racket[#t] when @racket[v] is a
 shrink-tree value and @racket[make-shrink-tree] creates a new shrink-tree from
 a value  @racket[v], and a promise of a list of shrinks @racket[shrs].
}

@defproc[(build-shrink-tree [v any/c] [shr (-> any/c (listof any/c))]) shrink-tree?]{
 Creates a shrink-tree by recursively applying the given shrinking function
 @racket[shr] to @racket[v]. Useful for constructing a shrink-tree from a
 standard shrinking function. Use @racket[gen:with-shrink] for this
 functionality without having to create a custom generator.
}

@defproc[(shrink-tree-map [f (-> any/c any/c)] [st shrink-tree?]) shrink-tree?]{
 Maps @racket[f] onto all the values contained within @racket[st].
}

@defproc[(gen:const [v any/c]) gen?]{
  Creates a generator that always returns @racket[v].
}

@defproc[(gen:map [g gen?]
                  [f (-> any/c any/c)]) gen?]{

  Creates a generator that transforms the values generated by
  @racket[g] by applying them to @racket[f] before returning them.
}

@defproc[(gen:bind [g gen?]
                   [f (-> any/c gen?)]) gen?]{

  Creates a generator that depends on the values produced by @racket[g].

  When shrinking, the shrinking of the output of @racket[g] is prioritized
  over the shrinking of the output of @racket[g].

  @ex[
  (define gen:list-of-trues
    (gen:bind
     gen:natural
     (lambda (len)
       (gen:list-len (gen:const #t) len))))
  ]

  @ex[
  (sample gen:list-of-trues 5)
  ]

  @ex[
  (full-shrink gen:list-of-trues 5 #:max-depth 1)
  ]
}

@defproc[(gen:filter [g gen?]
                     [p (-> any/c boolean?)]
                     [max-attempts (or/c exact-positive-integer? +inf.0) 1000])
         gen?]{

  Produces a generator that repeatedly generates values using
  @racket[g] until the result of @racket[p] applied to one of those
  values is @racket[#t] or the number of attempts exceeds
  @racket[max-attempts].

  An exception is raised when the generator runs out of attempts.

  This is a very brute-force way of generating values and you should
  avoid using it as much as possible, especially if the range of
  outputs is very small compared to the domain.  Take a generator for
  non-empty strings as an example.  Instead of:

  @ex[
  (gen:filter
   (gen:string gen:char-alphanumeric)
               (lambda (s)
                (not (string=? s ""))))
  ]

  Write:

  @ex[
  (gen:let ([hd gen:char-alphanumeric]
            [tl (gen:string gen:char-alphanumeric)])
   (string-append (string hd) tl))
  ]

  The latter takes a little more effort to write, but it doesn't
  depend on the whims of the random number generator and will always
  generate a non-empty string on the first try.
}

@defproc[(gen:choice [g gen?] ...+) gen?]{
  Produces a generator that generates values by randomly choosing one
  of the passed-in generators.
}

@defproc[(gen:sized [f (-> exact-nonnegative-integer? gen?)]) gen?]{
  Creates a generator that uses the generator's @racket[size] argument.
}

@defproc[(gen:resize [g gen?] [size exact-nonnegative-integer?]) gen?]{
  Creates a generator that overrides the generator's size with @racket[size].
}

@defproc[(gen:scale [g gen?]
                    [f (-> exact-nonnegative-integer?
                           exact-nonnegative-integer?)])
         gen?]{
  Creates a generator that modifies the size by applying @racket[f].
}

@defproc[(gen:no-shrink [g gen?]) gen?]{
  Creates a generator based on @racket[g] that never shrinks.
}

@defproc[(gen:with-shrink [g gen?] [shr (-> any/c (listof any/c))]) gen?]{
  Creates a generator that overrides the default shrinking with recursive
  applications of @racket[shr] to the generator output.
}

@defform[(gen:let ([id gen-expr] ...+) body ...+)]{
  Provides a convenient syntax for creating generators that depend on
  one or more other generators.

  @ex[
  (define gen:list-of-trues-2
    (gen:let ([len gen:natural])
      (make-list len #t)))
  ]

  @ex[
  (sample gen:list-of-trues-2 5)
  ]

  @ex[
  (full-shrink gen:list-of-trues-2 5 #:max-depth 5)
  ]
}

@defform[(gen:delay gen-expr)]{
  Creates a generator that wraps and delays the execution of
  @racket[gen-expr] until it is called.  This is handy for when you
  need to write recursive generators.
}

@subsubsection{Basic Generators}

@defthing[gen:natural gen?]{
  Generates natural numbers.

  @ex[(sample gen:natural)]
  @ex[(full-shrink gen:natural #:max-depth 1)]
}

@defproc[(gen:integer-in [lo exact-integer?]
                         [hi exact-integer?]) gen?]{

  Creates a generator that produces exact integers between @racket[lo]
  and @racket[hi], inclusive.  A contract error is raised if
  @racket[lo] is greater than @racket[hi].

  @ex[(sample (gen:integer-in 1 255))]
  @ex[(full-shrink (gen:integer-in 1 255) #:max-depth 1)]
}

@defthing[gen:real gen?]{
  Generates real numbers between @racket[0] and @racket[1], inclusive.
  Real numbers do not currently shrink, but this may change in the
  future.

  @ex[(sample gen:real)]
  @ex[(full-shrink gen:real)]
}

@defproc[(gen:one-of [xs (non-empty-listof any/c)]) gen?]{
  Creates a generator that produces values randomly selected from
  @racket[xs].

  @ex[(define gen:letters (gen:one-of '(a b c)))]
  @ex[(sample gen:letters)]
  @ex[(full-shrink gen:letters #:max-depth 1)]
}

@defthing[gen:boolean gen?]{
  Generates boolean values.

  @ex[(sample gen:boolean)]
  @ex[(full-shrink gen:boolean #:max-depth 1)]
}

@defthing[gen:char gen?]{
  Generates ASCII characters.

  @ex[(sample gen:char)]
  @ex[(full-shrink gen:char #:max-depth 1)]
}

@defthing[gen:char-letter gen?]{
  Generates ASCII letters.

  @ex[(sample gen:char-letter)]
  @ex[(full-shrink gen:char-letter #:max-depth 1)]
}

@defthing[gen:char-digit gen?]{
  Generates ASCII digits.

  @ex[(sample gen:char-digit)]
  @ex[(full-shrink gen:char-digit #:max-depth 1)]
}

@defthing[gen:char-alphanumeric gen?]{
  Generates alphanumeric ASCII characters.

  @ex[(sample gen:char-alphanumeric)]
  @ex[(full-shrink gen:char-alphanumeric #:max-depth 1)]
}

@defproc[(gen:tuple [g gen?] ...+) gen?]{
  Creates a generator that produces heterogeneous lists where the
  elements are created by generating values from each @racket[g] in
  sequence.

  @ex[(sample (gen:tuple gen:natural gen:boolean))]
  @ex[(full-shrink (gen:tuple gen:natural gen:boolean) #:max-depth 1)]
}

@defproc[(gen:list [g gen?]
                   [#:max-length max-len exact-nonnegative-integer? 128]) gen?]{

  Creates a generator that produces lists of random lengths where every
  element is generated using @racket[g]. Shrinks by removing elements from the
  list, then by shrinking individual elements of the list.

  @ex[(sample (gen:list gen:natural) 5)]
  @ex[(full-shrink (gen:list gen:natural) 5 #:max-depth 1)]
}

@defproc[(gen:list-len [g gen?] [len exact-nonnegative-integer?]) gen?]{
  Like @racket[gen:list], but produces lists of exactly @racket[len] elements.
}

@defproc[(gen:vector [g gen?]
                     [#:max-length max-len exact-nonnegative-integer? 128]) gen?]{

  Like @racket[gen:list] but for @racket[vector?]s.

  @ex[(sample (gen:vector gen:natural) 5)]
  @ex[(full-shrink (gen:vector gen:natural) 5 #:max-depth 1)]
}

@defproc[(gen:bytes [g gen? (gen:integer-in 0 255)]
                    [#:max-length max-len exact-nonnegative-integer? 128]) gen?]{

  Like @racket[gen:list] but for @racket[bytes?]s.  Raises a contract
  error if @racket[g] produces anything other than integers in the
  range 0 to 255 inclusive.

  @ex[(sample (gen:bytes) 5)]
  @ex[(full-shrink (gen:bytes) 5 #:max-depth 1)]
}

@defproc[(gen:string [g gen? gen:char]
                     [#:max-length max-len exact-nonnegative-integer? 128]) gen?]{

  Like @racket[gen:list] but for @racket[string?]s.  Raises a contract
  error if @racket[g] produces anything other than @racket[char?]
  values.

  @ex[(sample (gen:string gen:char-letter) 5)]
  @ex[(full-shrink (gen:string gen:char-letter) 5 #:max-depth 1)]
}

@defproc[(gen:symbol [g gen? gen:char]
                     [#:max-length max-len exact-nonnegative-integer? 128]) gen?]{

  Like @racket[gen:string] but for @racket[symbol?]s.  Raises a
  contract error if @racket[g] produces anything other than
  @racket[char?] values.

  @ex[(sample (gen:symbol gen:char-letter) 5)]
  @ex[(full-shrink (gen:symbol gen:char-letter) 5 #:max-depth 1)]
}

@deftogether[(
  @defproc[(gen:hash    [k any/c] [g gen?] ...+ ...+) gen?]
  @defproc[(gen:hasheq  [k any/c] [g gen?] ...+ ...+) gen?]
  @defproc[(gen:hasheqv [k any/c] [g gen?] ...+ ...+) gen?]
)]{

  These functions create generators that produce @racket[hash]es,
  @racket[hasheq]s and @racket[hasheqvs], respectively, where each key
  maps to a value generated from its associated generator.

  @ex[(sample (gen:hasheq 'a gen:natural 'b (gen:string gen:char-letter)) 5)]
  @ex[(full-shrink (gen:hasheq 'a gen:natural 'b (gen:string gen:char-letter)) #:max-depth 1)]
}

@defproc[(gen:frequency [frequencies (non-empty-listof (cons/c exact-positive-integer? gen?))]) gen?]{

  Creates a generator that generates values using a generator that is
  randomly picked from @racket[frequencies].  Generators with a higher
  weight will get picked more often.

  @ex[(sample (gen:frequency `((5 . ,gen:natural)
                               (2 . ,gen:boolean))))]
  @ex[(full-shrink (gen:frequency `((5 . ,gen:natural)
                                    (2 . ,gen:boolean)))
                   #:max-depth 1)]
}

@subsubsection{Unicode Generators}
@defmodule[rackcheck/gen/unicode]

@deftogether[(
  @defthing[gen:unicode gen?]
  @defthing[gen:unicode-letter gen?]
  @defthing[gen:unicode-mark gen?]
  @defthing[gen:unicode-number gen?]
  @defthing[gen:unicode-punctuation gen?]
  @defthing[gen:unicode-symbol gen?]
  @defthing[gen:unicode-separator gen?]
)]{

  These generators produce valid unicode @racket[char?]s.

  @ex[(sample gen:unicode)]
  @ex[(sample gen:unicode-letter)]
  @ex[(sample gen:unicode-mark)]
  @ex[(sample gen:unicode-number)]
  @ex[(sample gen:unicode-punctuation)]
  @ex[(sample gen:unicode-symbol)]
  @ex[(sample gen:unicode-separator)]
}

@subsection{Properties}

@defproc[(property? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a property.
}

@defform[(property [#:name name symbol? 'unnamed] ([id gen-expr] ...) body ...+)]{
  Declares a property where the inputs are one or more generators.

  @ex[
    (property ([xs (gen:list gen:natural)])
      (check-equal? (reverse (reverse xs)) xs))
  ]
}

@defform[(define-property name
          ([id gen-expr] ...)
          body ...+)]{

  A shorthand for @racket[(define name (property ...))].
}

@defform[(check-property maybe-config prop-expr)
         #:grammar ([maybe-config (code:line)
                                  (code:line config-expr)])]{

  Tries to falsify the property @racket[p] according to the config.
  If not provided, then a default configuration with a random seed
  value is used.

  @ex[
    (check-property
     (property ([xs (gen:list gen:natural)])
       (check-equal? (reverse (reverse xs)) xs)))
  ]

  @ex[
     (check-property
      (property ([xs (gen:list gen:natural)])
        (check-equal? (reverse xs) xs)))
  ]
}

@defproc[(label! [s (or/c false/c string?)]) void?]{
  Keeps track of how many times @racket[s] appears in the current set
  of tests.  Use this to classify and keep track of what categories
  the inputs to your properties fall under.

  Does nothing when @racket[s] is @racket[#f].

  @ex[
  (check-property
   (property
    ([a gen:natural]
     [b gen:natural])
    (label!
     (case a
      [(0)  "zero"]
      [else "non-zero"]))
    (+ a b)))
  ]
}

@defproc[(config? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a config value.
}

@defproc[(make-config [#:seed seed (integer-in 0 (sub1 (expt 2 31))) ...]
                      [#:tests tests exact-positive-integer? 100]
                      [#:size size (-> exact-positive-integer? exact-nonnegative-integer?) (lambda (n) (expt (sub1 n) 2))]
                      [#:deadline deadline (>=/c 0) (+ (current-inexact-milliseconds) (* 60 1000))]) config?]{

  Creates values that control the behavior of @racket[check-property].
}
