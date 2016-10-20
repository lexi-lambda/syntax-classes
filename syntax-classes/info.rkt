#lang info

(define version "1.1")

(define collection 'multi)

(define deps
  '("base"
    "syntax-classes-lib"
    "syntax-classes-doc"))
(define build-deps
  '())

(define implies
  '("syntax-classes-lib"
    "syntax-classes-doc"))
