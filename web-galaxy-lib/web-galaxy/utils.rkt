#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse)
  racket/contract/base
  racket/path
  racket/string
  racket/syntax
  syntax/stx)

(provide/contract
  [filename (-> path? string?)]
  [stx-map (-> (-> syntax? syntax?)
               (or/c syntax? (listof syntax?))
               syntax?)]
  [format-prefix (-> string? syntax? syntax?)]
  [format-suffix (-> syntax? string? syntax?)])

(provide
  cond/list
  cond/string)

(define (filename path)
  (path->string
    (path-replace-extension
      (file-name-from-path path)
      #"")))

(define (stx-map func l)
  (datum->syntax l (map func (stx->list l))))

(define (format-prefix str stx)
  (format-id stx (string-append str "~a") stx))

(define (format-suffix stx str)
  (format-id stx (string-append "~a" str) stx))

(begin-for-syntax
  (define-syntax-class maybe-cond
    #:literals (_)
    (pattern (_ value:expr)
      #:with condition #'#t)
    (pattern (condition:expr value:expr))))

(define-syntax (cond/list stx)
  (syntax-parse stx
    [(cond/list mc:maybe-cond ...)
     #'(let* ([result null]
              [result (if mc.condition
                          (cons mc.value result)
                          result)] ...)
         (reverse result))]))

(define-syntax (cond/string stx)
  (define-splicing-syntax-class maybe-sep
    (pattern (~seq #:separator sep:str))
    (pattern (~seq) #:with sep #'" "))
  (syntax-parse stx
    [(_ mc:maybe-cond ... ms:maybe-sep join-options ...)
     #'(string-join (cond/list mc ...) ms.sep join-options ...)]))

(module+ test
  (require
    rackunit)

  (define beer 'la-chouffe)
  (define (fresh? beer) #t)
  (define (open-bottle beer) 'opened-beer)
  (define time 'afternoon)

  (define things-i-like
    (cond/list
      [_ 'carpaccio]
      [_ 'pasta]
      [(eq? time 'morning) 'croissant]
      [(eq? time 'afternoon) 'brioche]
      [#f 'salsifi]
      [(fresh? beer) (open-bottle beer)]))

  (check-equal? things-i-like
                '(carpaccio pasta brioche opened-beer))

  (check-equal? (cond/string
                  [_ "I really"]
                  [_ "love"]
                  [#f "hate"]
                  [_ "food"]
                  #:separator " * "
                  #:after-last "!")
                "I really * love * food!")

  )
