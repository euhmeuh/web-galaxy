#lang racket/base

(provide
  current-response-headers
  current-error-responder
  current-not-found-responder
  req
  req-data
  req-cookie
  redirect/permanently
  redirect/temporarily
  redirect/see-other
  redirect/cookie
  response/page
  response/raw
  response/file
  response/json
  response/not-found
  response/full
  response/output
  response/xexpr
  define-response
  response-parameterize)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse)
  racket/match
  racket/port
  racket/stxparam
  syntax/parse/define
  json
  web-server/http/cookie
  web-server/http/cookie-parse
  web-server/servlet
  "translate.rkt")

(define current-response-headers (make-parameter '()))

(define-simple-macro (response-parameterize ([h-name:id h-value:expr] ...) body:expr ...+)
  (parameterize ([current-response-headers
                  (append (current-response-headers)
                          (list (list 'h-name h-value) ...))])
    body ...))

(define (response-headers->headers-list headers)
  (map (lambda (a-header)
         (header (string->bytes/utf-8 (symbol->string (car a-header)))
                 (string->bytes/utf-8 (cadr a-header))))
       headers))

(define-syntax-parameter req
  (lambda (stx)
    (raise-syntax-error 'req "Used outside define-response" stx)))

(define (response/page content)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    content))

(define (response/raw #:code [code 200]
                      #:message [message #"OK"]
                      #:mimetype [mimetype TEXT/HTML-MIME-TYPE]
                      bytes)
  (response/full
    code message (current-seconds) mimetype
    (response-headers->headers-list (current-response-headers))
    (list bytes)))

(define (response/file #:mimetype [mimetype TEXT/HTML-MIME-TYPE]
                       file)
  (response/full
    200 #"OK" (current-seconds) mimetype
    (response-headers->headers-list (current-response-headers))
    (port->bytes-lines (open-input-file file) #:close? #t)))

(define (response/json content)
  (response/full
    200 #"OK" (current-seconds) #"application/json; charset=utf-8"
    (response-headers->headers-list (current-response-headers))
    (list (jsexpr->bytes content))))

(define (response/not-found content)
  (response/xexpr
    #:code 404
    #:message #"Not Found"
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
    404 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE
    (response-headers->headers-list (current-response-headers))
    '(#"404 - Not found")))

(define (response-error url exception)
  (log-error "~s" `((exn ,(exn-message exception))
                    (uri ,(url->string url))
                    (time ,(current-seconds))))
  (response/full
    500 #"Internal Server Error" (current-seconds) TEXT/HTML-MIME-TYPE
    (response-headers->headers-list (current-response-headers))
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

(define (req-cookie request key)
  (define the-cookie
    (findf
      (lambda (cookie) (string=? key (client-cookie-name cookie)))
      (request-cookies request)))
  (and the-cookie
       (client-cookie-value the-cookie)))

(define (redirect/permanently uri [headers '()])
  (redirect-to uri permanently #:headers headers))

(define (redirect/temporarily uri [headers '()])
  (redirect-to uri temporarily #:headers headers))

(define (redirect/see-other uri [headers '()])
  (redirect-to uri see-other #:headers headers))

(define (redirect/cookie uri key value #:secure? [secure? #t])
  (redirect/see-other uri (list (cookie->header (make-cookie key value #:secure? secure?)))))
