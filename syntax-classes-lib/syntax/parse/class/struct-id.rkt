#lang racket/base

(require racket/require)

(require (multi-in racket [list struct-info])
         (rename-in syntax/parse [attribute @])
         syntax/parse/class/local-value
         syntax/parse/experimental/specialize)

(provide struct-id)

(define-syntax-class/specialize local-value/struct-info
  (local-value struct-info? #:name "structure type"))

(define-syntax-class struct-id
  #:description #f
  #:commit
  #:attributes [local-value info
                descriptor-id constructor-id predicate-id all-fields-visible? supertype-id
                num-fields num-supertype-fields num-own-fields [field-sym 1] [own-field-sym 1]
                [accessor-id 1] [mutator-id 1] [own-accessor-id 1] [own-mutator-id 1]]
  (pattern :local-value/struct-info
    #:attr info (extract-struct-info (@ local-value))

    #:attr descriptor-id (first (@ info))
    #:attr constructor-id (second (@ info))
    #:attr predicate-id (third (@ info))
    #:attr supertype-id (sixth (@ info))

    #:do [(define accessor-ids (reverse (fourth (@ info))))
          (define mutator-ids (reverse (fifth (@ info))))]
    #:attr all-fields-visible? (and (or (empty? accessor-ids) (first accessor-ids)) #t)
    #:attr [accessor-id 1] (if (@ all-fields-visible?)
                               accessor-ids
                               (rest accessor-ids))
    #:attr [mutator-id 1] (if (@ all-fields-visible?)
                              mutator-ids
                              (rest mutator-ids))

    #:attr num-fields (length (@ accessor-id))
    #:attr num-supertype-fields
           (if (identifier? (@ supertype-id))
               (let ([supertype-info (extract-struct-info (syntax-local-value #'supertype-id))])
                 (count identifier? (fourth supertype-info)))
               0)
    #:attr num-own-fields (- (@ num-fields) (@ num-supertype-fields))
    #:attr [own-accessor-id 1] (take-right (@ accessor-id) (@ num-own-fields))
    #:attr [own-mutator-id 1] (take-right (@ mutator-id) (@ num-own-fields))

    #:attr [own-field-sym 1] (and (struct-field-info? (@ local-value))
                                  (reverse (struct-field-info-list (@ local-value))))
    #:attr [field-sym 1] (and (@ own-field-sym)
                              (prepend-super-fields (@ own-field-sym) (@ supertype-id)))))

(define (prepend-super-fields fields super-id)
  (cond
    [(identifier? super-id)
     (define super-info (syntax-local-value super-id (λ () #f)))
     (and (struct-info? super-info)
          (struct-field-info? super-info)
          (prepend-super-fields
           (append (reverse (struct-field-info-list super-info)) fields)
           (sixth (extract-struct-info super-info))))]
    [else
     fields]))

