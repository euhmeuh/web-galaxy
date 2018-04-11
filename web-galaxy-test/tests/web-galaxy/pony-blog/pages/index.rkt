#lang racket/base

(provide
  index-page)

(require
  "_base.rkt")

(define (index-page articles [title "Home"])
  (base-page title '()
    (lambda ()
      `(main
         (h2 "Welcome to my pony blog!")))))
