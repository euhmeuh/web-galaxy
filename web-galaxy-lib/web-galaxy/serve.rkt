#lang racket/base

(provide
  number-arg
  integer-arg
  real-arg
  string-arg
  symbol-arg
  current-server-cert
  current-server-key
  current-server-listen-ip
  current-server-port
  current-server-root-path
  current-server-static-paths
  serve/all
  server-command-line)

(require
  racket/cmdline
  syntax/parse/define
  (for-syntax racket/base)
  web-server/servlet
  web-server/servlet-env
  web-server/managers/none
  web-server/configuration/responders
  "site-mode.rkt"
  "response.rkt")

(define current-server-cert (make-parameter #f))
(define current-server-key (make-parameter #f))
(define current-server-listen-ip (make-parameter (if-debug "127.0.0.1" #f)))
(define current-server-port (make-parameter (if-debug 8000 80)))
(define current-server-root-path (make-parameter (current-directory)))
(define current-server-static-paths (make-parameter '()))

(begin-for-syntax
  (define-syntax-class method-exp
    (pattern (~datum GET))
    (pattern (~datum HEAD))
    (pattern (~datum POST))
    (pattern (~datum PUT))
    (pattern (~datum DELETE))
    (pattern (~datum TRACE))
    (pattern (~datum OPTIONS))
    (pattern (~datum CONNECT))
    (pattern (~datum PATCH))))

(define-simple-macro
  (serve/all (method:method-exp (endpoint arg ...) response) ...)
  #:with (method-string ...) (datum->syntax
                               #'(method ...)
                               (map (lambda (m)
                                      (string-downcase
                                        (symbol->string (syntax-e m))))
                                    (syntax->list #'(method ...))))
  (begin
    (define-values
      (dispatcher url-maker)
      (dispatch-rules ((endpoint arg ...) #:method method-string response) ...))
    (serve/servlet
      dispatcher
      #:command-line? #t
      #:banner? #t
      #:servlet-regexp #rx""
      #:listen-ip (current-server-listen-ip)
      #:port (current-server-port)
      #:manager (create-none-manager (current-not-found-responder))
      #:servlet-responder (if-debug servlet-error-responder (current-error-responder))
      #:server-root-path (current-server-root-path)
      #:extra-files-paths (current-server-static-paths)
      #:file-not-found-responder (current-not-found-responder)
      #:ssl? (and (current-server-cert) (current-server-key) #t)
      #:ssl-cert (current-server-cert)
      #:ssl-key (current-server-key)
      #:log-file (current-output-port)
      #:log-format 'extended)))

(define-simple-macro
  (server-command-line (~alt (~optional (~seq #:program name))
                             (~optional (~seq #:argv argv))
                             (~seq clause:keyword spec ...)
                             ) ...
                       (~or (~seq #:args arg-formals body ...+)
                            (~seq)))
  (command-line
    (~? (~@ #:program name))
    (~? (~@ #:argv argv))
    #:once-each
    [("--address") address
     "Listen on a specific IP address"
     (current-server-listen-ip address)]
    [("--cert") cert
     "Specify the SSL server certificate"
     (current-server-cert cert)]
    [("--key") key
     "Specify the SSL server key"
     (current-server-key key)]
    [("--port") port-arg
     "Open the server on a specific port"
     (let ([port (string->number port-arg)])
       (if (and port
                (exact-positive-integer? port)
                (port . <= . 65535))
          (current-server-port port)
          (raise-user-error 'wrong-port
                            "Port should be an integer between 1 and 65535 (given: ~a)"
                            port-arg)))]
    (~@ (~@ clause spec ...) ...)
    (~? (~@ #:args arg-formals body ...))))

(module+ test
  (require
    racket/port
    rackunit)

  (define-binary-check (check-string=? string=? actual expected))

  (define current-database-path (make-parameter #f))
  (define current-server-name (make-parameter #f))
  (define current-server-mood (make-parameter #f))

  (define help-string #<<'''
my-program [ <option> ... ]
 where <option> is one of
  --address <address> : Listen on a specific IP address
  --cert <cert> : Specify the SSL server certificate
  --key <key> : Specify the SSL server key
  --port <port-arg> : Open the server on a specific port
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'

'''
)

  (parameterize ([current-server-port #f]
                 [current-server-listen-ip #f]
                 [current-server-cert #f]
                 [current-server-key #f]
                 [current-command-line-arguments
                  #("--port" "8080"
                    "--address" "1.2.3.4"
                    "--cert" "my-cert.pem"
                    "--key" "my-key.pem")])
    (server-command-line)
    (check-equal? (current-server-port) 8080)
    (check-equal? (current-server-listen-ip) "1.2.3.4")
    (check-equal? (current-server-cert) "my-cert.pem")
    (check-equal? (current-server-key) "my-key.pem"))

  (parameterize ([current-database-path #f]
                 [current-server-port #f]
                 [current-command-line-arguments
                  #("--port" "1234" "-xxxxx"
                    "--database" "database.sqlite")])
    (server-command-line
      #:multi
      [("-x")
       "Do something"
       (void)]
      #:once-each
      [("-d" "--database") database-path
       "Path to the SQLite database"
       (current-database-path database-path)])
    (check-equal? (current-server-port) 1234)
    (check-equal? (current-database-path) "database.sqlite"))

  (parameterize ([exit-handler (lambda (x) #t)])
    (check-string=?
      (with-output-to-string
        (lambda ()
          (server-command-line #:program "my-program"
                               #:argv #("--help"))))
      help-string))

  (parameterize ([current-server-name #f]
                 [current-server-mood #f])
    (server-command-line
      #:argv #("dumbo" "fuzzy")
      #:args (name mood)
      (current-server-name name)
      (current-server-mood mood))
    (check-equal? (current-server-name) "dumbo")
    (check-equal? (current-server-mood) "fuzzy"))

)
