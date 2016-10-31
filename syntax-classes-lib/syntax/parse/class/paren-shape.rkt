#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse)

(provide paren-shape
         paren-shape/parens paren-shape/brackets paren-shape/braces
         ~parens ~brackets ~braces)

(define (syntax-property/car stx prop)
  (let ([val (syntax-property stx prop)])
    (if (pair? val)
        (car val)
        val)))

(define-syntax-class (paren-shape shape)
  #:description #f
  [pattern _ #:when (equal? (syntax-property/car this-syntax 'paren-shape) shape)])

(define-syntax-class paren-shape/parens
  #:description "list or pair surrounded by parentheses"
  #:opaque
  [pattern {~var _ (paren-shape #f)}])

(define-syntax-class paren-shape/brackets
  #:description "list or pair surrounded by square brackets"
  #:opaque
  [pattern {~var _ (paren-shape #\[)}])

(define-syntax-class paren-shape/braces
  #:description "list or pair surrounded by curly braces"
  #:opaque
  [pattern {~var _ (paren-shape #\{)}])

(define-syntax ~parens
  (pattern-expander
   (syntax-parser
     [(_ . pat)
      #'{~and :paren-shape/parens pat}])))

(define-syntax ~brackets
  (pattern-expander
   (syntax-parser
     [(_ . pat)
      #'{~and :paren-shape/brackets pat}])))

(define-syntax ~braces
  (pattern-expander
   (syntax-parser
     [(_ . pat)
      #'{~and :paren-shape/braces pat}])))
