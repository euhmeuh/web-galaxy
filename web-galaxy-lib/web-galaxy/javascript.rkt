#lang racket/base

(provide
  javascript
  (all-from-out urlang)
  (all-from-out urlang/extra))

(require
  syntax/parse
  syntax/parse/define
  urlang
  urlang/extra)

(define-urlang-macro function
  (lambda (stx)
    (syntax-parse stx
      [(function (func-name arg ...) body ...)
       (syntax/loc stx
         (var [func-name (lambda (arg ...) body ...)]))])))

(define-simple-macro
  (javascript name jsexpr ...)
  (let ([js (open-output-string)])
    (parameterize ([current-urlang-run? #f]
                   [current-urlang-echo? #t]
                   [current-output-port js])
      (urlang
        (urmodule name
          (import XMLHttpRequest encodeURIComponent Promise JSON)
          (var [name ((lambda () jsexpr ...))]))))
    (get-output-string js)))
