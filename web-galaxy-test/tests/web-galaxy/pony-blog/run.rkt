#lang racket/base

(require
  web-galaxy/response
  web-galaxy/serve
  "models/article.rkt"
  "pages/index.rkt"
  "pages/article.rkt")

(define articles
  (list (article 'favorite-pony "My favorite pony is..." 1523560464 '(pony favorite personal)
                 '((p "Rarity of course. She's the fanciest and most generous pony around.")
                   (p "What about you? What is your favorite pony?")))
        (article 'being-a-brony "Being a brony" 1522550464 '(brony pony personal)
                 '((p "I've met a lot of awesome people being a brony.")
                   (p "It was fun. I still keep that part of my life in a little box in my heart.")))))

(define-response (index)
  (response/page (index-page articles)))

(define-response (article id)
  (define article (find-article-by-id articles (string->symbol id)))
  (if article
      (response/page (article-page article))
      (current-not-found-responder)))

(define-response (tags id)
  (response/page (index-page (filter-articles-by-tag articles id)
                             (format "Articles tagged ~a" id))))

(serve/all
  [GET ("") response-index]
  [GET ("article" (string-arg)) response-article]
  [GET ("tag" (symbol-arg)) response-tags])
