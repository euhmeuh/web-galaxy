#lang info

(define collection 'multi)
(define deps '("db-lib"
               "srfi-lite-lib"
               "base" "anaphoric" "web-server-lib"))
(define build-deps '("rackunit-lib"))
(define pkg-desc "Implementation (no documentation) part of \"web-galaxy\"")
(define pkg-authors '(euhmeuh))
(define version "0.1")
