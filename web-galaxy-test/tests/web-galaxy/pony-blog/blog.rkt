#lang racket/base

(require
  web-galaxy/response
  web-galaxy/serve
  "models/article.rkt")

(define articles '())

(define-response (index)
  (local-require "pages/index.rkt")
  (response-page (index-page (get-recent-articles articles))))

(define-response (article id)
  (local-require "pages/article.rkt")
  (response-page (article-page id)))

(define-response (tags id)
  (local-require "pages/index.rkt")
  (response-page (index-page (filter-articles-by-tag articles id)
                             (format "Articles tagged ~a" id))))

(serve/all
  [("") response-index]
  [("article" (string-arg)) response-article]
  [("tags" (symbol-arg)) response-tags])
