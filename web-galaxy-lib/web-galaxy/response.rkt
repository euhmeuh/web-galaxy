#lang racket/base

(provide
  current-error-responder
  current-not-found-responder
  req
  response-page
  define-response)

(require
  (for-syntax racket/base
              racket/syntax
              syntax/parse)
  racket/stxparam
  web-server/servlet
  "translate.rkt")

(define-syntax-parameter req
  (lambda (stx)
    (raise-syntax-error stx 'req "Used outside define-response")))

(define-syntax-rule (response-page content)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    content))

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
