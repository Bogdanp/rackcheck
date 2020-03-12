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
  id integer primary key autoincrement,
  name text not null,
  score integer not null default 0
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
    (for/list ([(id name score) (in-query (current-conn) "SELECT * FROM players ORDER BY id")])
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

(define-values (app _)
  (dispatch-rules
   [("players") players]
   [("players") #:method "post" add-player]))

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

  (current-conn (sqlite3-connect #:database 'memory))
  (init-db)

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

  (define gen:player-name
    (gen:let ([given (gen:string gen:char-letter)]
              [family (gen:string gen:char-letter)])
      (format "~a ~a" given family)))

  (run-tests
   (test-suite
    "web-api"
    #:before
    (lambda ()
      (define ch (make-async-channel))
      (set! stop (start ch))
      (sync ch))

    #:after
    (lambda ()
      (stop))

    (test-case "adding players"
      (struct model (names)
        #:transparent)

      (define gen:ops
        (gen:let ([names (gen:no-shrink
                          (gen:resize (gen:filter (gen:list gen:player-name) (compose1 not null?)) 5))]
                  [ops (gen:list
                        (gen:choice
                         (gen:tuple (gen:const 'create) (gen:one-of names))
                         (gen:tuple (gen:const 'count))))])
          (cons '(init) ops)))

      (define/match (interpret s op)
        [(s (list 'init))
         (reset)
         (model null)]

        [((model names) (list 'create name))
         (define (create-player)
           (with-handlers ([exn:fail? void])
             (request "/players" (hasheq 'name name))))

         (cond
           [(regexp-match-exact? " *" name)
            (begin0 s
              (create-player))]

           [(member name names)
            (begin0 s
              (create-player))]

           [else
            (begin0 (model (cons name names))
              (create-player))])]

        [((model names) (list 'count))
         (begin0 s
           (check-equal? (length (request "/players"))
                         (length names)))])

      (check-property
       (property ([ops gen:ops])
         (for/fold ([s #f])
                   ([op (in-list ops)])
           (interpret s op))))))))
