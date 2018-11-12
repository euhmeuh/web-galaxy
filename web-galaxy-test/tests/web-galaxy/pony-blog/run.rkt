#lang racket/base

(require
  web-galaxy/response
  web-galaxy/serve
  "models/article.rkt")

(define articles
  (list (article 'favorite-pony "My favorite pony is..." 1523560464 '(pony favorite personal)
                 '((p "Rarity of course. She's the fanciest and most generous pony around.")
                   (p "What about you? What is your favorite pony?")))
        (article 'being-a-brony "Being a brony" 1522550464 '(brony pony personal)
                 '((p "I've met a lot of awesome people being a brony.")
                   (p "It was fun. I still keep that part of my life in a little box in my heart.")))))

(define-response (index)
  (local-require "pages/index.rkt")
  (response/page (index-page articles)))

(define-response (article id)
  (local-require "pages/article.rkt")
  (define article (find-article-by-id articles (string->symbol id)))
  (if article
      (response/page (article-page article))
      (current-not-found-responder)))

(define-response (tags id)
  (local-require "pages/index.rkt")
  (response/page (index-page (filter-articles-by-tag articles id)
                             (format "Articles tagged ~a" id))))

(serve/all
  [("") response-index]
  [("article" (string-arg)) response-article]
  [("tag" (symbol-arg)) response-tags])
