#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/class/struct-id)
         rackunit
         rackunit/spec)

(describe "class struct-id"
  (struct boring ())
  (struct parent (a b))
  (struct child parent (a b))

  (it "handles structs with no fields"
    (define-syntax get-struct-info
      (syntax-parser [(_ id:struct-id)
                      #'id.descriptor-id]))
    (check-equal? (get-struct-info boring) struct:boring))

  (describe "attribute accessor-id"
    (it "includes all accessors, including parent accessors"
      (define-syntax struct-accessors
        (syntax-parser [(_ id:struct-id)
                        #'(list id.accessor-id ...)]))
      (check-equal? (struct-accessors parent)
                    (list parent-a parent-b))
      (check-equal? (struct-accessors child)
                    (list parent-a parent-b child-a child-b))))

  (describe "attribute own-accessor-id"
    (it "includes accessors, but not parent accessors"
      (define-syntax struct-own-accessors
        (syntax-parser [(_ id:struct-id)
                        #'(list id.own-accessor-id ...)]))
      (check-equal? (struct-own-accessors parent)
                    (list parent-a parent-b))
      (check-equal? (struct-own-accessors child)
                    (list child-a child-b)))))
