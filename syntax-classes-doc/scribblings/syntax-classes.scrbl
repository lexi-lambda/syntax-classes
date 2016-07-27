#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     racket/function
                     syntax/parse
                     syntax/parse/class/local-value
                     syntax/parse/class/struct-id
                     syntax/parse/experimental/template)
          scribble/eval)

@(define (guide-tech . pre-content)
   (apply tech #:doc '(lib "scribblings/guide/guide.scrbl") pre-content))

@(define (syntax-tech . pre-content)
   (apply tech #:doc '(lib "syntax/scribblings/syntax.scrbl") pre-content))

@(define (syntax-class-tech . pre-content)
   (apply tech #:doc '(lib "syntax/scribblings/syntax.scrbl") #:key "syntax class" pre-content))

@(define (make-syntax-class-eval)
   (let ([eval ((make-eval-factory '()))])
     (eval '(require (for-syntax racket/base
                                 syntax/parse
                                 syntax/parse/class/local-value
                                 syntax/parse/class/struct-id
                                 syntax/parse/experimental/template)))
     eval))

@(define-syntax-rule (syntax-class-examples body ...)
   (examples #:eval (make-syntax-class-eval)
             body ...))

@title{More Syntax Classes}

This library provides additional @syntax-class-tech{syntax classes} for use with
@racketmodname[syntax/parse].

@section{Locally bound transformer bindings}

@defmodule[syntax/parse/class/local-value]

@defform[#:kind "syntax class"
         (local-value @#,elem{[@racket[_predicate?]]} @#,elem{[@racket[#:failure-message _failure-message]]})
         #:contracts ([_predicate? @#,elem{@racket[(any/c . -> . any/c)] = @racket[(const #t)]}]
                      [_failure-message @#,elem{@racket[(or/c string? #f)] = @racket[#f]}])]{
A @syntax-class-tech{syntax class} for parsing identifiers bound to @guide-tech{transformer bindings}. It
parses an identifier, then calls @racket[syntax-local-value] on it and binds the result to an
attribute named @tt{local-value}.

If @racket[_predicate?] is specified, then @racket[_predicate?] will be applied to the result of
@racket[syntax-local-value], and if the result is @racket[#f], then the syntax class will fail to
match.

If the identifier is not bound to a @guide-tech{transformer binding}, or if the binding does not
satisfy @racket[_predicate?], then @racket[_failure-message] will be used as the error message, if it
is supplied.

@(syntax-class-examples
  (define-syntax print-local
    (syntax-parser
      [(_ id:local-value)
       (println (attribute id.local-value))
       #'(void)]))
  (define-syntax something 42)
  (print-local something)

  (define-syntax print-local-string
    (syntax-parser
      [(_ {~var id (local-value string?)})
       (println (attribute id.local-value))
       #'(void)]))
  (print-local-string something)

  (define-syntax print-local-string/message
    (syntax-parser
      [(_ {~var id (local-value string? #:failure-message "identifier was not bound to a string")})
       (println (attribute id.local-value))
       #'(void)]))
  (print-local-string/message something))}

@section{Structure type transformer bindings}

@defmodule[syntax/parse/class/struct-id]

@defidform[#:kind "syntax class" struct-id]{
A @syntax-class-tech{syntax class} for parsing
@seclink["structinfo" #:doc '(lib "scribblings/reference/reference.scrbl")]{structure type transformer
bindings}. Like the @racket[local-value] syntax class, it will parse an identifier, then call
@racket[syntax-local-value] on it to get a value. This syntax class will only match if the resulting
value satisfies @racket[struct-info?], and it will then bind a set of attributes:

@itemlist[
  @item{The @tt{info} attribute is bound to the list form of the @racket[struct-info?] value (that is,
        the value produced by calling @racket[extract-struct-info] on the transformer value).}

  @item{The @tt{descriptor-id} attribute is bound to an identifier that is bound to the structure
        type’s descriptor, or @racket[#f] if none is known.}

  @item{The @tt{constructor-id} attribute is bound to an identifier that is bound to the structure
        type’s constructor, or @racket[#f] if none is known.}

  @item{The @tt{predicate-id} attribute is bound to an identifier that is bound to the structure
        type’s predicate, or @racket[#f] if none is known.}

  @item{The @tt{all-fields-visible?} attribute is bound to @racket[#t] if all structure fields are
        visible to the macro, otherwise it is @racket[#f].}

  @item{The @tt{num-fields} attribute is bound to an exact, nonnegative integer that describes the
        number of visible fields the structure type has, including supertype fields.}

  @item{The @tt{accessor-id} attribute is an attribute of @syntax-tech{ellipsis depth} 1 that is bound
        to identifiers bound to accessors for all visible structure fields, including supertype
        fields.}

  @item{The @tt{mutator-id} attribute is like @tt{accessor-id}, except that it contains identifiers
        bound to mutators instead of accessors. It is guaranteed to have the same number of elements
        as @tt{accessor-id}; however, the value will be @racket[#f] for each non-mutable field.}

  @item{The @tt{supertype-id} attribute is bound to an identifier or a boolean. If it is an
        identifier, then the identifier is a structure type transformer binding for the structure’s
        supertype. If it is @racket[#t], then the structure has no supertype. If it is @racket[#f],
        then the structure’s supertype is unknown.}

  @item{The @tt{num-supertype-fields} attribute is like @tt{num-fields}, except that it only counts
        supertype fields, not fields that belong to the structure type itself.}

  @item{The @tt{num-own-fields} attribute is like @tt{num-fields}, except that it does not count
        supertype fields, only fields that belong to the structure type itself.}

  @item{The @tt{own-accessor-id} attribute is like @tt{accessor-id}, except that it does not include
        supertype fields, only fields that belong to the structure type itself.}

  @item{The @tt{own-mutator-id} attribute is like @tt{mutator-id} combined with the
        supertype-excluding behavior of @tt{own-accessor-id}.}]

Due to the nature of the @tt{mutator-id} attribute, it can be useful to use @racket[template] from
@racketmodname[syntax/parse/experimental/template] instead of @racket[syntax] when using mutator ids.

@(syntax-class-examples
  (define-syntax struct-accessors+mutators
    (syntax-parser
      [(_ id:struct-id)
       (template
        '((id.accessor-id (?? id.mutator-id #f))
          ...))]))
  (struct foo (bar [baz #:mutable] qux))
  (struct-accessors+mutators foo))}
