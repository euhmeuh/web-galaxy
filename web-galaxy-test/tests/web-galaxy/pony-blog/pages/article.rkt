#lang racket/base

(provide
  article-page)

(require
  racket/list
  "_base.rkt"
  "../models/article.rkt")

(define (render-tag-link tag)
  `(a ([href ,(format "/tag/~a" tag)])
      ,(symbol->string tag)))

(define (article-page article)
  (base-page (article-title article) '()
    (lambda ()
      `(main
         (h2 ,(article-title article))
         (h3 "Tags:" ,@(add-between (map render-tag-link (article-tags article)) 'nbsp))
         ,@(article-content article)))))
