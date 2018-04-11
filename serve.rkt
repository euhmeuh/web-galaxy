#lang racket/base

(provide
  current-server-listen-ip
  current-server-port
  current-server-root-path
  current-server-static-paths
  serve/all)

(require
  web-server/servlet
  web-server/servlet-env
  web-server/managers/none
  web-server/configuration/responders
  "site-mode.rkt"
  "response.rkt")

(define-syntax-rule (serve/all ((endpoint arg ...) response) ...)
  (begin
    (define-values
      (dispatcher url-maker)
      (dispatch-url ((endpoint arg ...) response) ...))
    (serve/servlet
      dispatcher
      #:command-line? #t
      #:banner? #t
      #:servlet-regexp #rx""
      #:listen-ip (current-server-listen-ip)
      #:port (current-server-port)
      #:manager (create-none-manager (current-response-not-found))
      #:servlet-responder (if-debug servlet-error-responder (current-response-error))
      #:server-root-path (current-server-root-path)
      #:extra-files-paths (current-server-static-paths)
      #:file-not-found-responder (current-response-not-found)
      ;;#:log-file (current-output-port)
      ;;#:log-format 'extended
      )))
