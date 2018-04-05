#lang racket/base

(provide
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
