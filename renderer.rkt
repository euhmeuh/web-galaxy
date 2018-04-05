#lang racket/base

(provide
  gen:renderer
  define-renderer
  render-element
  (struct-out container)
  render-elements)

(require
  (for-syntax racket/base
              racket/syntax
              syntax/parse)
  racket/generic)

(define-generics renderer
  (render renderer))

(define-syntax (define-renderer stx)
  (define-splicing-syntax-class name-maybe-parent
    (pattern (~seq name:id parent:id))
    (pattern name:id))
  (syntax-parse stx
    [(_ nmp:name-maybe-parent (field ...) body ...)
     #:with function-name (format-id stx "render-~a" #'nmp.name)
     #`(begin
         (struct #,@ #'nmp (field ...)
          #:methods gen:renderer
          [(define (render nmp.name)
            ((function-name) nmp.name))])
         (define function-name
           (make-parameter (lambda (nmp.name) body ...))))]))

(define (render-element element)
  (cond
    [(renderer? element) (render element)]
    [else element]))

(struct container (elements))

(define (render-elements container)
  (map render-element (container-elements container)))
