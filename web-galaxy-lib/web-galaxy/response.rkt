#lang racket/base

(provide
  current-error-responder
  current-not-found-responder
  req
  req-data
  response/page
  response/file
  response/json
  response/full
  response/output
  response/xexpr
  define-response)

(require
  (for-syntax racket/base
              racket/syntax
              syntax/parse)
  racket/match
  racket/port
  racket/stxparam
  json
  web-server/servlet
  "translate.rkt")

(define-syntax-parameter req
  (lambda (stx)
    (raise-syntax-error 'req "Used outside define-response" stx)))

(define-syntax-rule (response/page content)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    content))

(define-syntax-rule (response/file file)
  (response/full
    200 #"OK" (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    (port->bytes-lines (open-input-file file) #:close? #t)))

(define-syntax-rule (response/json content)
  (response/full
    200 #"OK" (current-seconds) #"application/json; charset=utf-8"
    '()
    (list (jsexpr->bytes content))))

(define-syntax (define-response stx)
  (syntax-parse stx
    [(_ (name arg ...) body ...)
     #:with func-name (format-id stx "response-~a" #'name)
     #`(define (func-name request arg ...)
         (syntax-parameterize ([req (make-rename-transformer #'request)])
           (parameterize ([current-language (request-language req)])
             body ...)))]))

(define-response (not-found)
  (response/full
    404 #"Not found"
    (current-seconds) TEXT/HTML-MIME-TYPE '()
    '(#"404 - Not found")))

(define (response-error url exception)
  (log-error "~s" `((exn ,(exn-message exception))
                    (uri ,(url->string url))
                    (time ,(current-seconds))))
  (response/full
    500 #"Internal server error"
    (current-seconds) TEXT/HTML-MIME-TYPE '()
    '(#"500 - Internal server error")))

(define current-not-found-responder (make-parameter response-not-found))
(define current-error-responder (make-parameter response-error))

(define (req-data key request)
  (match
    (bindings-assq
      (string->bytes/utf-8 key)
      (request-bindings/raw request))
    [(? binding:form? b)
       (bytes->string/utf-8
         (binding:form-value b))]
    [_ #f]))
