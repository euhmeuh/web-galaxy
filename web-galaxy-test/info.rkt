#lang info

(define collection 'multi)
(define deps '())
(define build-deps '("base"
                     "rackunit-lib"
                     "web-galaxy-lib"))
(define update-implies '("web-galaxy-lib"))
(define pkg-desc "Tests for \"web-galaxy\"")
(define pkg-authors '(euhmeuh))
