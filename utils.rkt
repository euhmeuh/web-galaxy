#lang racket/base

(require
  racket/contract/base)

(provide/contract
  [filename (-> path? string?)])

(require
  racket/path)

(define (filename path)
  (path->string
    (path-replace-extension
      (file-name-from-path path)
      #"")))
