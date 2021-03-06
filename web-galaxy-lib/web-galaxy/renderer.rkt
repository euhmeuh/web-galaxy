#lang racket/base

(provide
  current-custom-renderers
  gen:renderer
  define-renderer
  render-element
  (struct-out container)
  render-elements
  define-simple-container)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse
    web-galaxy/utils)
  syntax/parse/define
  racket/generic)

(define-generics renderer
  (render renderer))

(define-syntax (define-renderer stx)
  (define-splicing-syntax-class name-maybe-parent
    (pattern (~seq name:id parent:id))
    (pattern name:id))
  (define-syntax-class field-exp
    (pattern (name option:keyword ...))
    (pattern name:id))
  (syntax-parse stx
    [(_ nmp:name-maybe-parent (field:field-exp ...) body ...)
     #:with function-name (format-prefix "render-" #'nmp.name)
     #:with (field-getter ...)
            (stx-map (lambda (field)
                       (define field-func (format-id field "~a-~a" #'nmp.name field))
                       #`(#,field-func nmp.name))
                     #'(field.name ...))
     #`(begin
         (struct #,@ #'nmp (field ...)
          #:methods gen:renderer
          [(define (render nmp.name)
            ((function-name) nmp.name))])
         (define function-name
           (make-parameter
             (lambda (nmp.name)
               (let ([field.name field-getter] ...)
                 body ...)))))]))

(define current-custom-renderers (make-parameter '()))

(define (get-custom-renderer element)
  (ormap (lambda (rule)
           (let ([predicate (car rule)]
                 [renderer (cdr rule)])
             (if (predicate element)
                 renderer
                 #f)))
         (current-custom-renderers)))

(define (render-element element)
  (cond
    [(renderer? element) (render element)]
    [(get-custom-renderer element) => (lambda (r) (r element))]
    [else element]))

(struct container (elements))

(define (render-elements container)
  (map render-element (container-elements container)))

(define-simple-macro (define-simple-container name)
  (define-renderer name container ()
    `(name ,@(render-elements name))))
