#lang racket/base

(provide
  define-entity)

(require
  (for-syntax
    racket/base
    syntax/parse
    web-galaxy/utils)
  web-galaxy/renderer
  web-galaxy/db)

(define-syntax (define-entity stx)
  (syntax-parse stx
    [(_ <name>:id
        ([<field>:id <type>:id (~or (~seq <key>:keyword) (~seq))] ...)
        <body> ...)
     #'(begin
         (define-table <name> (<field> <type> (~? <key>)) ...)
         (define-renderer <name> (<field> ...) <body> ...)
         )]))

(define-syntax (entity-out stx)
  (syntax-parse stx
    [(_ <entity>:id)
     #:with render-<entity> (format-prefix "render" #'<entity>)
     #'(begin
         (struct-out <entity>)
         (table-out <entity>)
         render-<entity>)]))
