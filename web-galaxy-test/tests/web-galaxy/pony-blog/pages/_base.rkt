#lang racket/base

(provide
  base-page)

(require
  racket/string
  web-galaxy/entities
  web-galaxy/translate)

(define basic-links
  (list
    (link "Home" "/")
    (link "About" "#about")))

(define (render-navigation links)
  `(nav ([role "navigation"])
        (ul ,@(map (lambda (link)
                     `(li ,(render-element link)))
                   links))))

(define (render-title title)
  `(title ,(string-append title " | Pony Blog")))

(define (base-page title links renderer)
  `(html ([lang ,(symbol->string (current-language))])
     (head
       (meta ([charset "utf-8"]))
       (link ([rel "stylesheet"] [type "text/css"] [href "/common.css"]))
       ,(render-title title))
     (body
       (header
         (h1 "Pony Blog")
         (p "The blog about colorful ponies")
         ,(render-navigation (append basic-links links)))
       ,(renderer)
       (footer
         (p (small "This blog was made with web-galaxy, the Racket framework for the web!"))))))
