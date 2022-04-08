#lang info

(define version "1.3")

(define collection 'multi)

(define deps
  '())
(define build-deps
  '("base"
    "racket-doc"
    "scribble-lib"
    ["syntax-classes-lib" #:version "1.3"]))
