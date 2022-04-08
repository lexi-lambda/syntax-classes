#lang info

(define version "1.3")

(define collection 'multi)

(define deps
  '())
(define build-deps
  '("base"
    "rackunit-lib"
    "rackunit-spec"
    ["syntax-classes-lib" #:version "1.3"]))
