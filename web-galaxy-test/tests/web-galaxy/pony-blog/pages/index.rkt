#lang racket/base

(provide
  index-page)

(require
  "_base.rkt"
  "../models/article.rkt")

(define (render-article-link article)
  `(a ([href ,(format "/article/~a" (article-id article))])
      ,(article-title article)))

(define (index-page articles [title #f])
  (base-page (or title "Home") '()
    (lambda ()
      `(main
         (h2 ,(or title "Welcome to my pony blog!"))
         (h3 "Articles")
         (ul ,@(map (lambda (article)
                      `(li ,(render-article-link article)))
                    articles))))))
