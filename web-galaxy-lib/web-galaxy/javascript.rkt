#lang racket/base

(provide
  javascript)

(require
  syntax/parse
  syntax/parse/define
  urlang)

(define-simple-macro
  (javascript jsexpr ...)
  (let ([js (open-output-string)])
    (parameterize ([current-urlang-run? #f]
                   [current-urlang-echo? #t]
                   [current-output-port js])
      (urlang
        (urmodule jsmodule jsexpr ...)))
    (get-output-string js)))
