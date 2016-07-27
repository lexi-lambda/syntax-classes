#lang racket/base

(require data/maybe
         racket/contract
         racket/function
         syntax/parse)

(provide local-value)

(define-syntax-class (local-value [predicate? (const #t)] #:failure-message [message #f])
  #:description #f
  #:attributes [local-value]
  [pattern id:id
    #:do [(define maybe-local-value (exn->maybe exn:fail:contract? syntax-local-value #'id))]
    #:fail-unless (just? maybe-local-value) message
    #:attr local-value (from-just! maybe-local-value)
    #:fail-unless (predicate? (attribute local-value)) message])
