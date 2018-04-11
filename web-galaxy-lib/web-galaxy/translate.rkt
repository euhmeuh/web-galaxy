#lang racket/base

(require
  racket/contract/base
  web-server/http
  anaphoric)

(provide
  tr)

(provide/contract
  [load-translations! (-> void?)]
  [has-translations? (-> symbol? boolean?)]
  [translate (-> symbol? string?)]
  [current-language (parameter/c symbol?)]
  [current-country (parameter/c symbol?)]
  [current-translations-dir parameter?]
  [request-language (-> request? symbol?)])

(require
  racket/set
  srfi/29
  anaphoric
  "utils.rkt")

(define current-translations-dir (make-parameter "translations"))

(define bundle-name 'website)
(define languages (mutable-set))

(define (load-translations!)
  (for ([folder (in-directory (current-translations-dir))]
        #:when (directory-exists? folder))
    (let* ([language (string->symbol (filename folder))]
           [loaded? (load-bundle! (list bundle-name language)
                                  (current-translations-dir))])
      (when loaded?
        (set-add! languages language)))))

(define (has-translations? language)
  (set-member? languages language))

(define (translate id . args)
  (aif (localized-template bundle-name id)
       (apply format (cons it args))
       (format "/!\\~a/!\\" id)))

(define-syntax-rule (tr id arg ...)
  (translate 'id arg ...))

#|
  Get the language from the subdomain of the host name
|#
(define (request-language request)
  (define headers (request-headers/raw request))
  (define host (headers-assq* #"Host" headers))
  (define hostname (and host (header-value host)))
  (define language (and hostname (match-language hostname)))
  (if (and language
           (has-translations? language))
      language
      (current-language)))

(define (match-language hostname)
  (aif (regexp-match #rx"^([a-z]+)\\..+" hostname)
       (string->symbol
         (bytes->string/utf-8
           (cadr it))) ;; the second element is the first group matched
       #f))
