#lang racket/base

(provide
  number-arg
  integer-arg
  real-arg
  string-arg
  symbol-arg
  current-server-listen-ip
  current-server-port
  current-server-root-path
  current-server-static-paths
  serve/all)

(require
  syntax/parse/define
  (for-syntax racket/base)
  web-server/servlet
  web-server/servlet-env
  web-server/managers/none
  web-server/configuration/responders
  "site-mode.rkt"
  "response.rkt")

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
      #:log-file (current-output-port)
      #:log-format 'extended)))
