#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/class/struct-id)
         rackunit
         rackunit/spec)

(describe "class struct-id"
  (struct boring ())
  (struct parent (a b p))
  (struct child parent (a b c))

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
                    (list parent-a parent-b parent-p))
      (check-equal? (struct-accessors child)
                    (list parent-a parent-b parent-p child-a child-b child-c))))

  (describe "attribute own-accessor-id"
    (it "includes accessors, but not parent accessors"
      (define-syntax struct-own-accessors
        (syntax-parser [(_ id:struct-id)
                        #'(list id.own-accessor-id ...)]))
      (check-equal? (struct-own-accessors parent)
                    (list parent-a parent-b parent-p))
      (check-equal? (struct-own-accessors child)
                    (list child-a child-b child-c))))

  (describe "attribute own-fields"
    (it "includes field symbols, but not parent fields"
      (define-syntax struct-own-fields
        (syntax-parser [(_ id:struct-id)
                        #`'#,(attribute id.own-fields)]))
      (check-equal? (struct-own-fields boring)
                    (list))
      (check-equal? (struct-own-fields parent)
                    (list 'a 'b 'p))
      (check-equal? (struct-own-fields child)
                    (list 'a 'b 'c))))

  (describe "attribute all-fields"
    (it "includes field symbols, but not parent fields"
      (define-syntax struct-all-fields
        (syntax-parser [(_ id:struct-id)
                        #`'#,(attribute id.all-fields)]))
      (check-equal? (struct-all-fields boring)
                    (list))
      (check-equal? (struct-all-fields parent)
                    (list 'a 'b 'p))
      (check-equal? (struct-all-fields child)
                    (list 'a 'b 'p 'a 'b 'c)))))
