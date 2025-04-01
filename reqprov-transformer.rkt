#lang racket/base

(require "filters.rkt"
         racket/require-transform
         racket/provide-transform

         racket/list
         racket/base
         racket/sequence
         racket/syntax
         syntax/parse
         racket/contract)

(provide (struct-out auto-rename-transformer)
         make-auto-rename-transformer)

(define (apply-transforms xform im/exports)
  (define rf
    (xform
     (case-lambda
       [(im/ex acc) (cons im/ex acc)]
       [(acc) (reverse acc)])))
  (rf (foldl rf null im/exports)))

(define (call-wrapping-syntax-errors f . stxes)
  (define (wrap-err it)
    (writeln stxes)
    (exn:fail:syntax (exn-message it) (exn-continuation-marks it) stxes))

  (call-with-continuation-barrier
   (λ ()
     (with-handlers ([exn:fail? (λ (it) (raise (wrap-err it)))])
       (f)))))

(define (eval-transforms transform-stxes im/exports
                         #:orig-stx orig-stx
                         #:orig-exprs orig-transform-stxes
                         #:precompose [extra-transforms values])
  (define transforms
    (apply
     compose1
     extra-transforms
     (for/list ([tf (in-syntax transform-stxes)]
                [tf-stx (in-syntax orig-transform-stxes)])
       (call-wrapping-syntax-errors
        (λ () (syntax-local-eval tf))
        orig-stx tf-stx))))
  (call-wrapping-syntax-errors
   (λ () (apply-transforms transforms im/exports))
   orig-stx))

(define-splicing-syntax-class transformers
  #:attributes {[transformers 1] [orig-transformers 1]}
  [pattern {~optional {~seq {~and {~or #:tx #:transform} kw} txs ...}}
           #:declare txs (expr/c #'filter/c
                                 #:phase (add1 (syntax-local-phase-level))
                                 #:name (format "~a" (syntax-e #'kw)))
           #:with (orig-transformers ...) #'({~? {~@ txs ...}})
           #:with (transformers ...) #'({~? {~@ txs.c ...}})])

(define (extract-derived stx)
  (syntax-parse stx
    [(func #:derived orig-stx . tail)
     (values
      #'orig-stx
      (syntax/loc #'orig-stx (func . tail)))]
    [_ (values stx stx)]))

(define ((auto-rename-require-transform obj) stx)
  (define-values (orig-stx stx*) (extract-derived stx))
  (syntax-parse stx*
    #:context orig-stx
    [(_ {~describe #:opaque "require sub-form" form:expr}
        ...+
        tx:transformers)
     (define-values (imports sources)
       (for/lists (i s #:result (values (append* i) (append* s)))
                  ([it (in-syntax #'(form ...))])
         (expand-import it)))
     (values (eval-transforms #'(tx.transformers ...) imports
                              #:orig-stx orig-stx
                              #:orig-exprs #'(tx.orig-transformers ...)
                              #:precompose (auto-rename-transformer-builtins obj))
             sources)]))

(define ((auto-rename-provide-transform obj) stx modes)
  (define-values (orig-stx stx*) (extract-derived stx))
  (syntax-parse stx*
    #:context orig-stx
    [(_ {~describe #:opaque "provide sub-form" form:expr}
        ...+
        tx:transformers)
     (define exports
       (append*
        (for/list ([it (in-syntax #'(form ...))])
          (expand-export it modes))))
     (eval-transforms #'(tx.transformers ...)
                      exports
                      #:orig-stx orig-stx
                      #:orig-exprs #'(tx.orig-transformers ...)
                      #:precompose (auto-rename-transformer-builtins obj))]))

(struct auto-rename-transformer (builtins)
  #:extra-constructor-name new-auto-rename-transformer
  #:property prop:require-transformer auto-rename-require-transform
  #:property prop:provide-transformer auto-rename-provide-transform)

(define/contract (make-auto-rename-transformer . transforms)
  (filter/c ... . -> . auto-rename-transformer?)
  (new-auto-rename-transformer (apply compose1 transforms)))
