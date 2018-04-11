#lang racket/base

(provide
  article-page)

(require
  "_base.rkt")

(define (article-page id)
  (base-page "Article title" '()
    (lambda ()
      `(main
         (h2 "Great article")
         (p "Hello world!")))))
