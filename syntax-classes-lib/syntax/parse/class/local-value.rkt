#lang racket/base

(require racket/contract
         racket/function
         syntax/parse)

(provide local-value)

(define unbound-value
  (let ()
    (struct unbound-value ())
    (unbound-value)))

(define-syntax-class (local-value [predicate? (const #t)]
                                  [intdef-ctx #f]
                                  #:name [name #f]
                                  #:failure-message [message #f])
  #:description name
  #:commit
  #:attributes [local-value]
  [pattern id:id
           #:fail-unless (syntax-transforming?) "not currently expanding"
           #:attr local-value (syntax-local-value #'id (const unbound-value) intdef-ctx)
           #:fail-when (eq? (attribute local-value) unbound-value) message
           #:fail-unless (predicate? (attribute local-value)) message
           #:do [(syntax-parse-state-cons! 'literals #'id)]])
