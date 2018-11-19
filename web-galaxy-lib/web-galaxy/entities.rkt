#lang racket/base

(provide
  (all-from-out "renderer.rkt")

  (struct-out link)
  render-link

  (rename-out [make-pubdate pubdate])
  pubdate?
  pubdate-year
  pubdate-month
  pubdate-day
  render-pubdate
  format-pubdate

  pubdate<=?
  pubdate<?
  pubdate=?
  pubdate>=?
  pubdate>?

  (rename-out [make-p p])
  p?
  render-p

  (rename-out [make-div div])
  div?
  render-div

  (rename-out [make-strong strong])
  strong?
  render-strong

  newline)

(require
  "renderer.rkt")

(define-renderer link (url text)
  `(a ([href ,(link-url link)]) ,(link-text link)))

(define-renderer pubdate (year month day)
  (define day (pubdate-day pubdate))
  (define the-date (pubdate->date pubdate))
  `(time ([datetime ,(format-date the-date (if day 'iso 'iso-month))])
         ,(format-date the-date (if day 'full 'month))))

(define (make-pubdate year month [day #f])
  (pubdate year month day))

(define (pubdate->date pubdate)
  (local-require (only-in srfi/19 make-date))
  (make-date 0 0 0 0 (pubdate-day pubdate)
                     (pubdate-month pubdate)
                     (pubdate-year pubdate) 0))

(define ((make-pubdate-comparator comp) a b)
  (local-require (only-in srfi/19 date->julian-day))
  (comp (date->julian-day (pubdate->date a))
        (date->julian-day (pubdate->date b))))

(define pubdate<=? (make-pubdate-comparator <=))
(define pubdate<? (make-pubdate-comparator <))
(define pubdate=? (make-pubdate-comparator =))
(define pubdate>=? (make-pubdate-comparator >=))
(define pubdate>? (make-pubdate-comparator >))

(define pubdate-formats
  #hash([iso   . "~1"]
        [full  . "~A, ~B ~e, ~Y"]
        [iso-month . "~Y-~m"]
        [month . "~B ~Y"]))

(define (format-pubdate pubdate format)
  (format-date (pubdate->date pubdate) format))

(define (format-date date format)
  (local-require (only-in srfi/19 date->string))
  (date->string date (hash-ref pubdate-formats format)))

(define-simple-container p)
(define (make-p . elements) (p elements))

(define-simple-container div)
(define (make-div . elements) (div elements))

(define-simple-container strong)
(define (make-strong . elements) (strong elements))

(define (newline)
  '(br))
