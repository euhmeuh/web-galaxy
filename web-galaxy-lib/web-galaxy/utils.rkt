#lang racket/base

(require
  racket/contract/base
  racket/syntax
  syntax/stx)

(provide/contract
  [filename (-> path? string?)]
  [stx-map (-> (-> syntax? syntax?)
               (or/c syntax? (listof syntax?))
               syntax?)]
  [format-prefix (-> string? syntax? syntax?)]
  [format-suffix (-> syntax? string? syntax?)])

(require
  racket/path)

(define (filename path)
  (path->string
    (path-replace-extension
      (file-name-from-path path)
      #"")))

(define (stx-map func l)
  (datum->syntax l (map func (stx->list l))))

(define (format-prefix str stx)
  (format-id stx (string-append str "~a") stx))

(define (format-suffix stx str)
  (format-id stx (string-append "~a" str) stx))
