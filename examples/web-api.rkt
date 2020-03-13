#lang racket/base

(require db
         json
         racket/string
         web-server/dispatch
         web-server/http
         web-server/servlet-dispatch
         web-server/web-server)

(define current-conn
  (make-parameter #f))

(define (init-db)
  (query-exec (current-conn) #<<SQL
CREATE TABLE players(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  score INTEGER NOT NULL DEFAULT 0
);
SQL
              ))

(struct exn:fail:bad-request exn:fail ())

(define (bad-request message)
  (raise (exn:fail:bad-request message (current-continuation-marks))))

(define (request-json req)
  (with-handlers ([exn:fail:read?
                   (lambda _
                     (bad-request "failed to parse request JSON"))])
    (bytes->jsexpr (request-post-data/raw req))))

(define (response/json e #:code [code 200])
  (response/output
   #:code code
   #:mime-type #"application/json; charset=utf-8"
   (lambda (out)
     (write-json e out))))

(define (players req)
  (response/json
    (for/list ([(id name score) (in-query (current-conn) "SELECT * FROM players ORDER BY score DESC, name ASC")])
      (hasheq 'id id
              'name name
              'score score))))

(define (add-player req)
  (define data (request-json req))
  (unless (hash-has-key? data 'name)
    (bad-request "name field is required"))

  (define name (hash-ref data 'name))
  (when (string=? (string-trim name #:repeat? #t) "")
    (bad-request "name cannot be empty"))

  (call-with-transaction (current-conn)
    (lambda ()
      (define exists?
        (query-maybe-value (current-conn) "SELECT id FROM players WHERE name = ?" name))
      (when exists?
        (bad-request "a player with that name already exists"))

      (query-exec (current-conn) "INSERT INTO players (name) VALUES (?)" name)))

  (response/json (hasheq)))

(define (increase-score req name)
  (query-exec (current-conn) "UPDATE players SET score = score + 1 WHERE name = ?" name)
  (response/json (hasheq)))

(define-values (app _)
  (dispatch-rules
   [("players") players]
   [("players") #:method "post" add-player]
   [("scores" (string-arg)) #:method "post" increase-score]))

(define (start [ch #f])
  (serve
   #:confirmation-channel ch
   #:dispatch (dispatch/servlet (lambda (req)
                                  (with-handlers ([exn:fail:bad-request?
                                                   (lambda (e)
                                                     (response/json
                                                      #:code 400
                                                      (hasheq 'error (exn-message e))))])
                                    (app req))))
   #:port 9911))

(module+ test
  (require net/http-client
           rackcheck
           racket/async-channel
           racket/match
           racket/port
           rackunit
           rackunit/text-ui)

  (define stop #f)

  (define (reset)
    (query-exec (current-conn) "DELETE FROM players"))

  (define (request path [data #f])
    (define-values (status _headers out)
      (http-sendrecv "127.0.0.1" path
                     #:port 9911
                     #:method (if data #"POST" #"GET")
                     #:headers (if data '(#"Content-type: application/json") null)
                     #:data (and data (jsexpr->bytes data))))

    (match status
      [(regexp #rx#"^HTTP.... 200 ")
       (read-json out)]

      [(regexp #rx#"^HTTP.... 4.. ")
       (error 'client "bad request: ~s" (read-json out))]

      [_
       (error 'server (port->string out))]))

  (run-tests
   (test-suite
    "web-api"
    #:before
    (lambda ()
      (current-conn (sqlite3-connect #:database 'memory))
      (init-db)

      (define ch (make-async-channel))
      (set! stop (start ch))
      (sync ch))

    #:after
    (lambda ()
      (stop))

    (test-case "leaderboard"
      (struct model (scores-by-name)
        #:transparent)

      (define gen:player-name
        (gen:let ([given (gen:string gen:char-letter)]
                  [family (gen:string gen:char-letter)])
          (format "~a ~a" given family)))

      (define gen:ops
        (gen:let ([names (gen:no-shrink
                          (gen:resize (gen:filter (gen:list gen:player-name)
                                                  (compose1 not null?))
                                      10))]
                  [ops (gen:list
                        (gen:choice
                         (gen:tuple (gen:const 'create) (gen:one-of names))
                         (gen:tuple (gen:const 'increase) (gen:one-of names))))])
          (cons '(init) ops)))

      (define/match (interpret s op)
        [(s (list 'init))
         (reset)
         (model (hash))]

        [((model scores) (list 'create name))
         (define (create-player)
           (with-handlers ([exn:fail? void])
             (request "/players" (hasheq 'name name))))

         (define (player-names)
           (sort (for/list ([player (in-list (request "/players"))])
                   (hash-ref player 'name))
                 string<?))

         (define (scores->names s)
           (sort (hash-keys s) string<?))

         (cond
           [(regexp-match-exact? " *" name)
            (begin0 s
              (create-player)
              (check-equal? (player-names) (scores->names scores)))]

           [(hash-has-key? scores name)
            (begin0 s
              (create-player)
              (check-equal? (player-names) (scores->names scores)))]

           [else
            (define scores* (hash-set scores name 0))
            (begin0 (model scores*)
              (create-player)
              (check-equal? (player-names) (scores->names scores*)))])]

        [((model scores) (list 'increase name))
         (define scores*
           (if (hash-has-key? scores name)
               (hash-update scores name add1)
               scores))

         (request (format "/scores/~a" name) (hasheq))
         (check-equal?
          (for/list ([player (in-list (request "/players"))])
            (cons (hash-ref player 'name)
                  (hash-ref player 'score)))
          (sort (sort (hash->list scores*) string<? #:key car) > #:key cdr))
         (model scores*)])

      (check-property
       (make-config #:tests 30)
       (property ([ops gen:ops])
         (for/fold ([s #f])
                   ([op (in-list ops)])
           (interpret s op))))))))
