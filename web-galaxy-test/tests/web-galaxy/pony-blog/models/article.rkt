#lang racket/base

(provide
  (struct-out article)
  get-recent-articles
  find-article-by-id
  filter-articles-by-tag)

(struct article (id title timestamp tags content))

(define (last-month)
  (- (current-seconds) (* 31 24 60 60)))

(define (get-recent-articles articles)
  (filter (lambda (article)
            (> (article-timestamp article) (last-month)))
          articles))

(define (find-article-by-id articles id)
  (findf (lambda (article)
           (eq? (article-id article) id))
         articles))

(define (filter-articles-by-tag articles tag)
  (filter (lambda (article)
            (member tag (article-tags article)))
          articles))
