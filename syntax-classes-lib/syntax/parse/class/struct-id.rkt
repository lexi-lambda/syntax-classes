#lang racket/base

(require racket/require)

(require data/maybe
         (multi-in racket [bool list struct-info])
         syntax/parse
         syntax/parse/class/local-value
         syntax/parse/experimental/specialize)

(provide struct-id)

(define-syntax-class/specialize local-value/struct-info
  (local-value struct-info? #:failure-message "identifier is not bound to a structure type"))

(define-syntax-class struct-id
  #:description "structure type identifier"
  #:attributes [info descriptor-id constructor-id predicate-id all-fields-visible? supertype-id
                     [accessor-id 1] [mutator-id 1]]
  [pattern id:local-value/struct-info
    #:attr info (extract-struct-info (attribute id.local-value))
    #:attr descriptor-id (first (attribute info))
    #:attr constructor-id (second (attribute info))
    #:attr predicate-id (third (attribute info))
    #:attr all-fields-visible? (not (false? (last (fourth (attribute info)))))
    #:attr [accessor-id 1] (let ([accessor-ids (reverse (fourth (attribute info)))])
                             (if (attribute all-fields-visible?)
                                 accessor-ids
                                 (rest accessor-ids)))
    #:attr [mutator-id 1] (let ([mutator-ids (reverse (fifth (attribute info)))])
                            (if (attribute all-fields-visible?)
                                mutator-ids
                                (rest mutator-ids)))
    #:attr supertype-id (sixth (attribute info))])
