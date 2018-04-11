#lang racket/base

(provide
  if-debug
  production?
  site-is-production)

(define-syntax-rule (if-debug then else)
  (if (not site-is-production) then else))

(define (production?)
  (define mode (getenv "SITE_MODE"))
  (and (string? mode)
       (string=? mode "prod")))

(define site-is-production (production?))
